#!/usr/bin/env ruby

$my_dir = File.dirname(File.absolute_path(__FILE__))
$LOAD_PATH.unshift(File.join($my_dir, "../exe/lib"))

# $stderr.puts($LOAD_PATH.inspect)

require 'cgi'
require 'stringio'
require 'tempfile'
require 'tmpdir'
require 'fileutils'

require 'micro_macro_proc'
require 'my_config.rb'
require 'channel'

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


def timestamp
  t=Time.now;

  return t.tv_sec.to_s + t.tv_usec.to_s
end

def to_link(path)
  if (path[0...$my_dir.size] == $my_dir && path[$my_dir.size]='/')
    return path[$my_dir.size+1 .. -1]
  else
    raise "Trying to 'linkify' a path with the wrong form"
  end
end

def with_working_dir
  t = Time.now.sec
  status_dir = File.absolute_path(File.join($my_dir, "status/#{timestamp}"))

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

cgi=CGI.new("html4")

begin
  frame_rate = despace(cgi.params['fps'][0])
  raise Bad_Parameters, "Invalid fps" unless is_valid_time?(frame_rate)

  decay    = make_decay(cgi.params['decay'][0],
                        despace(cgi.params['tau'][0]))

  template = File.basename(cgi.params['template'][0])
  raise Bad_Parameters, "Bad template" unless is_valid_template?(template)
  
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


    Client_Side.new {
      |to_server|
      params.each {|p| to_server.puts(p)}
    }


    cgi.out do
      cgi.html do
        cgi.body do
          macros = { "working_dir" => working_dir,
                     "zipfile"     => to_link(zip_file),
                     "status_file" => to_link(progress_file),
                     "stderr"      => to_link(stderr_file)
                   }

          
          expanded_template(File.join($my_dir, '../exe/lib/working-for-you.thtml'),
                            macros)
          
        end
      end
    end

    # $stderr.puts(params.inspect)


  end
rescue Bad_Parameters => e
  create_error_page(cgi, "Bad request: #{e.message}")

rescue StandardError => e
  create_error_page(cgi, "Internal error (#{e.class}): #{e.message}")

end

# $stderr.puts (cgi.params.inspect)
# 
# cgi.print("Panini fritti!!!")
# cgi.print (cgi.params.inspect)
# 
# cgi.print(cgi.params['myfile'].first.read)
#   
# cgi.print(cgi.files.inspect)

# class CGIbis < CGI
#   include CGI::QueryExtension
# end
# 
# 
#  child_pid=fork
#
#  if child_pid
#    # I'm the parent.  Just exit
#    Process.detach(child_pid)
#  else
#    # I'm the child.  Do stderr redirection
#    $stdin.close
#    $stderr.close
#    $stdout.close
#    
#    $stderr=File.open(stderr_file, 'w')
#    $stdout=$stderr
#
#    exec(File.join($my_dir, "../exe/accumulator.exe"), *params)
#  end
    
#  
#
#  if status.success?
#    $stderr.puts('ok')
#    
#    create_success_page(cgi, dir, basename_template)
#
#  else
#    $stderr.puts('NO')
#    create_error_page(cgi, status, stderr)
#
#  end

# def create_success_page(cgi, dir, basename_template)
#   zip_path=create_zip_archive(dir, basename_template)
# 
#   $stderr.puts(cgi.inspect)
#   
#   cgi.out("type" => "application/zip",
#           # "disposition" => "attachment; filename=#{File.basename(zip_path)}"
#          ) {
#     File.read(zip_path)
#   }
# end

