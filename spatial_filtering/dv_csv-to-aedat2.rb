#!/usr/bin/env ruby

My_dir=File.dirname(File.expand_path($0))
$LOAD_PATH.unshift(My_dir)

require "dv_aedat"
require "dv_csv"

class Config
  def Config.parse_command_line
    @@filename_in  = ARGV.shift || "-"
    @@filename_out = ARGV.shift || "-"

    Config.die("Junk at end of the line: #{ARGV.join(' ')}") unless
      ARGV.empty?
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
    
    stream = if filename != "-"
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

Config.parse_command_line

SmartFile.open(Config.filename_in, :r) do |input|
  SmartFile.open(Config.filename_out, :w) do |output|

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
