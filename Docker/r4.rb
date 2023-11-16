#!/usr/bin/env ruby

# BEGIN HELP
# Help text
# More lines
# no skipping
# END HELP

class Immutable_Expansion < String
end

def extract_command(line)
  
  start = line.index('%{')

  return [line, nil, nil] unless start

  stop = line[start+2..-1].index('}%')

  if stop.nil?
    return [line, nil, nil] 
  else
    stop += start+2
    return [line[0...start], line[start+2...stop], line[stop+2..-1]]
  end
end

def execute_command(command, macros)
  name, val=command.split('=', 2)

  if val.nil?
    raise "Unknown name #{command}" unless macros.has_key?(command)
    return macros[command]

  else
    if ! macros[name].is_a?(Immutable_Expansion)
      macros[name]=val
    end

    return ""
  end
end

def prefill_from_cli(macros)
  ARGV.each do |arg|
    name, val=arg.split('=', 2)

    next if val.nil?

    macros[name]=Immutable_Expansion.new(val)
  end
end

def dump_help_text(target)
  myself=File.expand_path(__FILE__)

  in_help=false
  
  File.open(myself) do |stream|
    while line=stream.gets
      line.chomp!
      next unless line =~ /^ *#(.*)$/

      body=$1

      if body =~ /^ *BEGIN *HELP *$/
        in_help = true

      elsif body =~ /^ *END *HELP *$/
        in_help = false

      else
        target.puts(body) if in_help

      end
    end
  end
end

def do_help_if_asked
  if ARGV[0] == '--help' || ARGV[0] == '-h'
    dump_help_text($stderr)
    exit 0
  end
end

###
### MAIN
###

macros=Hash.new

do_help_if_asked

prefill_from_cli(macros)

while line = $stdin.gets
  line.chomp!

  loop do
    pre, command, post = extract_command(line)

    break if command.nil?

    replacement = execute_command(command, macros)

    line = pre + replacement + post
  end

  $stdout.puts(line)
end
