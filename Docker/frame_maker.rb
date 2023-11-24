#!/usr/bin/env -S ruby -I /usr/local/apache2/library

require 'cgi'
require 'stringio'
require 'tempfile'
require 'tmpdir'
require 'fileutils'
require 'logger'

ENV['HTTPD_PREFIX']='/usr/local/apache2'

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
    raise Bad_Parameters, "Bad tau specs" unless is_valid_time?(tau)
    return "linear:#{despace(tau)}"

  when "exp"
    raise Bad_Parameters, "Bad tau specs" unless is_valid_time?(tau)
    return "exp:#{despace(tau)}"

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
  if (path[0...$my_dir.size] == $my_dir && path[$my_dir.size]='/')
    return path[$my_dir.size+1 .. -1]
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

$logger.info("Starting")
cgi=CGI.new("html4")

begin
  $logger.info("Parameter checking")
  
  frame_rate = despace(cgi.params['fps'][0])
  raise Bad_Parameters, "Invalid fps" unless is_valid_time?(frame_rate)

  decay    = make_decay(cgi.params['decay'][0],
                        despace(cgi.params['tau'][0]))

  template = File.basename(cgi.params['template'][0])
  raise Bad_Parameters, "Bad template" unless is_valid_template?(template)

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

    full_template = File.join(image_dir, template)
    image_glob=template_to_glob(full_template)


    params = []
    params << "stderr:#{stderr_file}"
    params << "stdout:#{stdout_file}"
    params << "status:#{progress_file}"
    params << "images:#{image_glob}"
    params << "zip:#{zip_file}"
    params << "--sampling=#{frame_rate}"
    params << "--decay=#{decay}"
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
