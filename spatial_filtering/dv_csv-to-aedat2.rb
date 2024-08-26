#!/usr/bin/env ruby

require "dv_aedat"
require "dv_csv"

class SmartFile < File
  def SmartFile.open(filename, mode='r')
    raise RuntimeError unless
      filenams.is_a?(String) &&
      (mode.to_sym == :r || mode.to_sym == :w)
    
    stream = if filename != "-"
               File.open(filename, mode)
             else
               mode.to_sym == :r ? $stdin : $stdout
             end

    yield(stream)

    if filename != "-"
      stream.close
    end
  end
end

filename_in  = ARGV.shift || "-"
filename_out = ARGV.shift || "-"


SmartFile.open(filename_in, :r) do |input|
  SmartFile.open(filename_out, :w) do |output|

    events, errors, metadata = DV::IO_CSV::read(input)

    DV::IO_AEDAT_2::write(output, events)
  end
end
