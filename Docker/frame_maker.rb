#!/usr/bin/env -S ruby -I /usr/local/apache2/library

#
# This script is called via CGI when the user submit the form in
# index.html.  Its duty is to parse the parameters received
# via CGI and to pass them to worker.rb via an internal socket
#

MAX_INPUT_SIZE=1024 * 1024 * 1024 * 1024

ENV['HTTPD_PREFIX']='/usr/local/apache2'


require 'cgi'
require 'stringio'
require 'tempfile'
require 'tmpdir'
require 'fileutils'
require 'logger'

require 'micro_macro_proc'
require 'definitions'
require 'channel_client'


$logger=Logger.new(Tree.join(:log, 'cgi.log'))
$logger.level = Logger::INFO

def despace(x)
  raise "Bad despace parameter type" unless x.is_a?(String)

  x.tr(' ', '')
end

def save_events(events, target)
  raise "Events without newlines" unless events.include?("\n")
  raise "Target filename with newline" if target.include?("\n")
  
  File.open(target, 'w') {
    |output| output.write(events)
  }
end

def to_link(path)
  html_dir = Tree[:html] + '/'
  
  if (path[0...html_dir.size] == html_dir)
    return '/' + path[html_dir.size .. -1]
  else
    raise "Trying to 'linkify' a path with the wrong form"
  end
end

def get_timestamp
  t=Time.now;

  return t.strftime('%Y-%m-%d/%H:%M:%S%L')
  #return t.tv_sec.to_s + t.tv_usec.to_s
end

def with_working_dir
  status_dir = Tree.join(:job, get_timestamp)

  FileUtils.mkpath(status_dir)
  
  yield(status_dir)
end

def expanded_template(template, macros)
  expansion=Array.new
  
  Micro_Macro_Proc.expand(File.readlines(template),
                          expansion,
                          macros,
                          :on_undefined => :die)

  return expansion.join('')
end


class Bad_Parameters < RuntimeError
end

Parameters = Struct.new(:frame_rate,
                        :decay,
                        :template,
                        :max,
                        :min,
                        :neutral,
                        :rectify,
                        :lazy,
                        :weight,
                        :events)
class Blessed
  #
  #  Funny class, uh? Any value received via CGI is checked to see if
  #  it is a valid vale and then converted and the result used to create
  #  a value of class Blessed.  This acts like a "seal" that the value
  #  received from the outside has been processed and it should be
  #  safe now.
  #
  def initialize(value)
    @value=value
  end
  
  attr_reader :value
end

module Conversions
  def Conversions.is_valid_time?(t)
    return t =~ /^[0-9]+$/ ||
           t =~ /^[0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)?(s|ms|us|ns|fps)?$/ 
  end


  def Conversions.is_valid_template?(s)
    valid_chars = "-_.,:@a-zA-Z0-9"
    valid_template_regexp = Regexp.new("^[#{valid_chars}]*%d[#{valid_chars}]*$")

    return s =~ valid_template_regexp
  end

  def Conversions.template_to_glob(s)
    raise "I shouldn't be here" unless
      Conversions.is_valid_template?(File.basename(s))

    return Blessed.new(s.gsub(/%d/, '*'))
  end

  def Conversions.to_decay(decay, tau)
    case decay
    when "none", "step"
      return Blessed.new(decay)

    when "lin", "exp"
      tau = Conversions.to_time(tau)
      
      raise StandardError unless tau.is_a? Blessed
      
      tau=tau.value
      label = (decay == "lin") ? "linear" : "exp"
      
      return Blessed.new("#{label}:#{tau}")

    else
      raise Bad_Parameters, "Bad decay specs"
    end
  end

  def Conversions.to_time(s)
    raise Bad_Parameters, "Missing time" if s.nil?

    result = despace(s)

    raise Bad_Parameters, "Bad time spec" unless Conversions.is_valid_time?(s)

    return Blessed.new(result)
  end

  def Conversions.to_template(s)
    raise Bad_Parameters, "Missing template" if s.nil?
    
    result = File.basename(s)
    raise Bad_Parameters, "Bad template" unless
      Conversions.is_valid_template?(result)

    return Blessed.new(result)
  end

  def Conversions.to_float(s)
    raise Bad_Parameters, "Missing float" if s.nil?
    
    raise Bad_Parameters, "Invalid float" unless
      s =~ /^[-+]?[0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)?$/
    
    return Blessed.new(s.to_f)
  end

  def Conversions.to_boolean(s)
    return Blessed.new(s.nil? ? false : true)
  end
end

module External_Interface
  #
  #  In order to prevent data injection attacks, this module
  #  is the only part that is allowed to manipulate CGI data.
  #  In this way it is easier to control the impact of external data
  #
  def External_Interface.create_error_page(cgi, message)
    cgi.out do
      cgi.html do
        cgi.body do
          cgi.p do
            CGI::escapeHTML(
              message
            )
          end
        end
      end
    end
  end


  def External_Interface.extract_parameters(cgi)
    buffer = Parameters.new

    buffer.frame_rate = Conversions.to_time(cgi.params['fps'][0])

    buffer.decay = Conversions.to_decay(cgi.params['decay'][0],
                                        cgi.params['tau'][0])

    buffer.template = Conversions.to_template(cgi.params['template'][0])

    buffer.max = Conversions.to_float(cgi.params['max'][0])
    buffer.min = Conversions.to_float(cgi.params['min'][0])
    buffer.neutral = Conversions.to_float(cgi.params['neutral'][0])
    buffer.weight = Conversions.to_float(cgi.params['peso'][0])

    buffer.lazy = Conversions.to_boolean(cgi.params['lazy'][0])
    buffer.rectify = Conversions.to_boolean(cgi.params['rectify'][0])

    buffer.events = Blessed.new(cgi.params['myfile'][0].read)
    
    result = Parameters.new

    #
    # OK, now every parameter was read. Let's do a sanity check
    # - Every parameter was read
    # - Every value is Blessed
    # 
    buffer.each_pair do
      |member,blessed_value|

      raise "Member #{member} empty." if blessed_value.nil?

      raise "Member #{member} is not blessed" unless blessed_value.is_a? Blessed

      result[member]=blessed_value.value
    end

    return result
  end

  def External_Interface.with_received_data
    #
    #  This function read the data received via the CGI, sanitize
    #  and convert them and pass them to the block given to this function.
    #  We expect that the block will return (as the result of its last
    #  expression) an Hash table with some macros used to expand the
    #  output template.
    #
    #  This approach is not the most intuitive one, but in this way
    #  the CGI data do not exit from the External_Interface module,
    #  making it easier to avoid data injection attacks,
    #
    cgi=CGI.new(:tag_maker => "html4",
                :max_multipart_length => MAX_INPUT_SIZE)

    $logger.info("Parameter parsing")

    parameters = External_Interface.extract_parameters(cgi)

    macros = yield(parameters)

    $logger.info("Creating body")
    cgi.out do
      cgi.html do
        cgi.body do

          template=Tree.join(:lib, 'working-for-you.thtml')
          expanded_template(template, macros)
          
        end
      end
    end

  rescue Bad_Parameters => e
    External_Interface.create_error_page(cgi, "Bad request: #{e.message}")

  rescue StandardError => e
    External_Interface.create_error_page(cgi,
                                         "Internal error (#{e.class}): #{e.message}")
  end
end

###
### MAIN
###

$logger.info("Starting")

External_Interface.with_received_data do |parameters|
  $logger.info("Open working dir")
  
  with_working_dir do |working_dir|
    image_dir     = File.join(working_dir, 'images');
    FileUtils.mkpath(image_dir)
    
    progress_file = File.join(working_dir, 'progress')
    stderr_file   = File.join(working_dir, 'stderr')
    stdout_file   = File.join(working_dir, 'stderr')
    zip_file      = File.join(working_dir, 'frames.zip')
    event_file    = File.join(working_dir, 'events.csv')

    save_events(parameters.events, event_file)

    full_template = File.join(image_dir, parameters.template)
    image_glob=Conversions.template_to_glob(full_template)


    params = []
    params << "stderr:#{stderr_file}"
    params << "stdout:#{stdout_file}"
    params << "status:#{progress_file}"
    params << "images:#{image_glob}"
    params << "zip:#{zip_file}"
    params << "--sampling=#{parameters.frame_rate}"
    params << "--decay=#{parameters.decay}"
    params << "--min=#{parameters.min}"
    params << "--max=#{parameters.max}"
    params << "--neutral=#{parameters.neutral}"
    params << "--weight=#{parameters.weight}"
    params << "--rectify" if parameters.rectify
    params << "--lazy"    if parameters.lazy
    params << "--output=#{full_template}"
    params << "--input=#{event_file}"
    params << "--progress=#{progress_file}"

    $logger.info("Calling worker server")
    
    Client_Side.new {
      |to_server|
      $logger.info("Got it.  Read stuff")
      
      params.each {|p| to_server.puts(p)}

      $logger.info("Done")
    }


    $logger.info("Done")

    #
    # Hash table returned as value of this block
    #
    { "working_dir" => working_dir,
      "zipfile"     => to_link(zip_file),
      "status_file" => to_link(progress_file),
      "stderr"      => to_link(stderr_file)
    }
  end
end

