#!/usr/bin/env ruby

# BEGIN HELP
# Help text
# More lines
# no skipping
# END HELP

require './micro_macro_proc.rb'

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


do_help_if_asked


macros=Micro_Macro_Proc.prefill_from_cli(macros)

missing=Micro_Macro_Proc.expand($stdin, $stdout, macros)

if !missing.empty?
  $stderr.puts("Found undefined macros: #{missing.join(', ')}")
  exit 1
end

exit 0
