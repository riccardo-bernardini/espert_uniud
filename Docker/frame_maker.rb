#!/usr/bin/env -S ruby -I /usr/local/apache2/library

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
  x.is_a?(String) ? x.tr(' ', '') : x
end

def save_to(events, target)
  File.open(target, 'w') do
    |output|

    output.write(events.read)
    
  end
end

def is_valid_time?(t)
  return t =~ /^[0-9]+$/ ||
         t =~ /^[0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)?(s|ms|us|ns|fps)?$/ 
end

def is_valid_template?(s)
  invalid_chars = /[^-_.,:@%a-zA-Z0-9]/
  return false if s =~ invalid_chars

  pos = s.index('%d')
  return false if pos.nil?

  pos = s.index('%d', pos+2)
  return false unless pos.nil?

  return true
end

def template_to_glob(s)
  raise "I shouldn't be here" unless is_valid_template?(File.basename(s))

  return s.gsub(/%d/, '*')
end

def make_decay(decay, tau)
  case decay
  when "none", "step"
    return decay

  when "lin"
    return "linear:#{to_time(tau)}"

  when "exp"
    return "exp:#{to_time(tau)}"

  else
    raise Bad_Parameters, "Bad decay specs"
  end

end


def create_error_page(cgi, message)
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
                        :weight)

def to_time(s)
  raise Bad_Parameters if s.nil?

  result = despace(s)

  raise Bad_Parameters, "Bad time spec" unless is_valid_time?(s)

  return result
end

def to_template(s)
  raise Bad_Parameters if s.nil?
  
  result = File.basename(s)
  raise Bad_Parameters, "Bad template" unless is_valid_template?(result)

  return result
end

def to_float(s)
  raise Bad_Parameters if s.nil?

  raise Bad_Parameters unless s =~ /^[-+]?[0-9]+(\.[0-9]+([eE][-+]?[0-9]+)?)?$/
  
  return s.to_f
end

def to_boolean(s)
  return s.nil? ? false : true
end

def extract_parameters(cgi)
  result = Parameters.new

  result.frame_rate = to_time(cgi.params['fps'][0])

  result.decay = make_decay(cgi.params['decay'][0], cgi.params['tau'][0])

  result.template = to_template(cgi.params['template'][0])

  result.max = to_float(cgi.params['max'][0])
  result.min = to_float(cgi.params['min'][0])
  result.neutral = to_float(cgi.params['neutral'][0])
  result.weight = to_float(cgi.params['peso'][0])

  result.lazy = to_boolean(cgi.params['lazy'][0])
  result.rectify = to_boolean(cgi.params['rectify'][0])

  result.each_pair do
    |member,value|

    raise "Member #{member} empty. It should'nt be" if value.nil?
  end

  return result
end

$logger.info("Starting")
cgi=CGI.new(:tag_maker => "html4",
            :max_multipart_length => MAX_INPUT_SIZE)

begin
  $logger.info("Parameter parsing")

  parameters = extract_parameters(cgi)
  
  $logger.info("Open working dir")
  
  with_working_dir do |working_dir|
    image_dir     = File.join(working_dir, 'images');
    FileUtils.mkpath(image_dir)
    
    progress_file = File.join(working_dir, 'progress')
    stderr_file   = File.join(working_dir, 'stderr')
    stdout_file   = File.join(working_dir, 'stderr')
    zip_file      = File.join(working_dir, 'frames.zip')
    event_file    = File.join(working_dir, 'events.csv')

    save_to(cgi.params['myfile'][0], event_file)

    full_template = File.join(image_dir, parameters.template)
    image_glob=template_to_glob(full_template)


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
    params << "--lazy" if parameters.lazy
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

    $logger.info("Creating body")
    cgi.out do
      cgi.html do
        cgi.body do
          macros = { "working_dir" => working_dir,
                     "zipfile"     => to_link(zip_file),
                     "status_file" => to_link(progress_file),
                     "stderr"      => to_link(stderr_file)
                   }

          template=Tree.join(:lib, 'working-for-you.thtml')
          expanded_template(template, macros)
          
        end
      end
    end

    $logger.info("Done")


  end
rescue Bad_Parameters => e
  create_error_page(cgi, "Bad request: #{e.message}")

rescue StandardError => e
  create_error_page(cgi, "Internal error (#{e.class}): #{e.message}")

end
