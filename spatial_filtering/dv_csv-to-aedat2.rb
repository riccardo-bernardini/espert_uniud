#!/usr/bin/env ruby

require "pathname"

Myself = Pathname.new($0)
$LOAD_PATH.unshift(Myself.realdirpath.dirname)

require "dv_aedat"
require "dv_csv"

Standard_Stream_Name = "-"

class Config
  def Config.basename(path)
    if path == Standard_Stream_Name
      return "stdin"
    else
      path = File.basename(path)
      parts = path.split('.')

      if parts.size == 1
        return path
      else
        extension=parts[-1]
        
        return File.basename(path, ".#{extension}")
      end
    end
  end

  def Config.print_help
    $stderr.puts "Usage: #{File.basename($0)} [input] [output]"
    $stderr.puts
    $stderr.puts "  input and output default to '-' (stdin or stdout)"
    $stderr.puts "  If output is a directory, the filename default to"
    $stderr.puts "  input with extension .aedat"
    $stderr.puts
  end
  
  def Config.parse_command_line
    if ARGV.size > 0 && ["-h", "?", "--help"].include?(ARGV[0])
      Config.print_help
      exit 0
    end
    
    @@filename_in  = ARGV.shift || Standard_Stream_Name
    @@filename_out = ARGV.shift || Standard_Stream_Name

    Config.die("Junk at end of the line: #{ARGV.join(' ')}") unless
      ARGV.empty?

    Config.die("'#{@@filename_in}' does not exist or it is not a file") unless
      @@filename_in == Standard_Stream_Name ||
      File.file?(@@filename_in)

    if @@filename_out != Standard_Stream_Name
      path=Pathname.new(@@filename_out)

      if path.directory?
        #
        #  The second argument is a directory, use by default
        #  the name of the input with the extension replaced by
        #  aedat
        #
        filename = "#{Config.basename(@@filename_in)}.aedat"
        @@filename_out = File.join(@@filename_out, filename)

      elsif ! path.dirname.directory?
        #
        #  The second argument is not a directory which means that
        #  (1) it exists and it is a file or (2) it does not exist.
        #  In both cases, the part of path that precedes it must
        #  be an existing directory.  If I am here, it is not.
        #
        Config.die("Bad output path #{@@filename_out}")

      end
    end
  end

  def Config.filename_in
    @@filename_in
  end

  def Config.filename_out
    @@filename_out
  end

  def Config.flip_x
    true
  end

  def Config.flip_y
    true
  end

  def Config.die(msg)
    $stderr.puts(msg)
    exit 1
  end
end

class SmartFile < File
  def SmartFile.open(filename, mode='r')
    raise RuntimeError unless
      filename.is_a?(String) &&
      (mode.to_sym == :r || mode.to_sym == :w)
    
    stream = if filename != Standard_Stream_Name
               File.open(filename, mode.to_s)
             else
               mode.to_sym == :r ? $stdin : $stdout
             end

    yield(stream)

    if filename != "-"
      stream.close
    end
  end
end

###      ###
### MAIN ###
###      ###

Config.parse_command_line

SmartFile.open(Config.filename_in, :r) do |input|
  SmartFile.open(Config.filename_out, :w) do |output|

    if input.tty?
      $stderr.puts "Reading events from terminal."
      $stderr.print("This is quite unusual. Do you really want it? [y/N] ")

      require 'io/console'
      answer = STDIN.getch

      p answer
      
      unless answer.downcase == "y"
        $stderr.puts
        Config.print_help
        exit 0
      end
    end
    
    events, errors, metadata = DV::IO_CSV::read(input)

    if Config.flip_x || Config.flip_y
      x_size = (metadata['sizeX'] || '640').to_i
      y_size = (metadata['sizeY'] || '480').to_i
      
      events.each do
        |ev|

        ev.x.flip(x_size-1) if Config.flip_x
        ev.y.flip(y_size-1) if Config.flip_y
      end
    end


    DV::IO_AEDAT_2::write(output, events)
  end
end
