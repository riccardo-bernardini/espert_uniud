#!/usr/bin/env ruby

File_Title_Pair = Struct.new("File_Title_Pair", :filename, :title)

def get_title(filename)
  File.open(filename) do
    |input|

    input.each do
      |line|

      line.chomp!

      if line =~ /^ *# *([^ ].*)$/
        return $1
      end        
    end
  end

  return nil
end

if ARGV.size != 2
  $stderr.puts "Usage: $0 target-file source-list"
  exit 1
end

target_file=ARGV.shift
source_list=ARGV.shift.split(':')

pair_list=Array.new

source_list.each do
  |filename|

  next if filename.empty?

  title = get_title(filename)

  if title.nil?
    $stderr.puts "Warning: no title line found in #{filename}"

  else
    pair_list << File_Title_Pair.new(filename, title)
  end
end

File.open(target_file, 'w') do
  |output|

  output.puts "# Index"
  output.puts

  pair_list.each do
    |pair|

    basename = File.basename(pair.filename, '.md')
    output.puts "* [#{pair.title}](#{basename}.html)"
  end
end
