#!/usr/bin/env ruby

require 'optparse'

my_dir=File.dirname(File.realpath($0))

load(File.join(my_dir, 'event_files.rb'))

class Image
  Struct.new('Pair', 'x', 'y');

  #
  # A simple class that implements a "sparse image"
  #
  include Enumerable
  
  def initialize
    @pixels = Hash.new(0)
  end

  def [](x,y)
    @pixels[Struct::Pair.new(x,y)]
  end

  def []=(x,y,value)
    @pixels[Struct::Pair.new(x,y)]=value
  end

  def each
    #
    # Loop over all the non-zero pixel
    #
    @pixels.each do |key, val|
      yield(key.x, key.y, val)
    end
  end
end


def die(msg=nil)
  if msg
    $stderr.puts(msg)
  else
    $stderr.puts "Usage: #{$0} [options] filter"
    $stderr.puts "Read stdin, write stdout"
  end
  
  exit(1)
end

def parse_filter_row(line)
  entries = line.split

  mark_column = nil

  (0...entries.size).each do |idx|
    entry = entries[idx]
    
    if entry[-1] == '!'
      raise "Bad filter format" unless mark_column.nil?
      mark_column = idx;

      entry = entry[0...-1];
    end

    entries[idx] = entry.to_i
  end

  return [entries, mark_column]
end

def load_filter_from_file(filename)
  rows = Array.new
  origin = nil

  row_number = 0
  
  File.open(filename) do |input|
    input.each do |line|
      line.chomp!
      line.strip!
      
      next if line.empty? || line[0] == '#'

      this_row, marker_column = parse_filter_row(line)

      if marker_column
        raise "Bad filter format: double origin" unless origin.nil?
        origin = [row_number, marker_column]
      end

      unless rows.empty?
        raise "Bad filter format: different row length" if this_row.size != rows[0].size
      end

      row_number += 1
    end
  end

  raise "Bad filter format: missing origin" if origin.nil?

  filter = Image.new

  (0...rows.size).each do |y|
    row = rows[row_index]

    (0...row.size).each do |x|
      filter[x-origin[1], y-origin[0]] = row[x]
    end
  end

  return filter
end

def make_filter_circle(parameters)
  die("Circle: 1 parameter") unless parameters.size == 1
  
  radius = parameters[0].to_i

  die("Circle: radius = #{radius} not positive") unless radius > 0

  result = Image.new
  
  (-radius..radius).each do |x|
    y_range = Math.sqrt(radius*radius - x*x).to_i
    
    (-y_range..y_range).each do |y|
      result[x,y] = 1
    end
  end

  return result
end

def load_filter(spec)
  #
  # spec can be
  #
  #   filter_name:param1:param2:... (e.g., circle:10)
  #
  #   @filename
  #
  return load_filter_from_file(spec[1..-1]) if spec[0]=='@'

  name, *parameters = spec.split(':')
  
  case name.strip.downcase
  when "circle"
    return make_filter_circle(parameters)

  else
    die("Bad filter spec: '#{spec}'")
  end
end

  
def process_frame(frame, filter)
  t0 = frame[0].timestamp
  accumulator = Image.new

  frame.each do |event|
    raise "Bad, bad, bad" unless t0 == event.timestamp

    filter.each do |delta_x, delta_y, value|
      dst_x = event.x+delta_x
      dst_y = event.y+delta_y

      if dst_x > 0 && dst_y > 0
        accumulator[dst_x, dst_y] += event.polarity * value
      end
    end
  end

  return accumulator.map do |x, y, val|
    Event.new(t0, x, y, val, 'float')
  end
end

def parse_radix_spec(radix)
  extension_pos = radix.rindex('.')
  
  if extension_pos.nil?
    extension = 'csv'
    basename = radix
  else
    extension = radix[extension_pos+1 .. -1];
    basename  = radix[0 .. extension_pos-1]
  end

  head, tail, excess = basename.split('%w')

  raise "Too many %w" unless excess.nil?

  if tail.nil?
    tail = ''
    head = head + '_'
  end

  return [head, tail + '.' + extension]
end

def split_by_weight(event_file, radix)
  radix_head, radix_tail = parse_radix_spec(radix)

  files = collect_by_weight(event_file)

  files.each do |weight, events|
    events.each { |event| event.polarity = event.polarity > 0 ? 1 : 0 }
    
    output_event_file = Event_File.new(event_file)
    output_event_file.metadata['weight'] = weight
    output_event_file.metadata['polarity'] = 'boolean'
    output_event_file.events.concat(events)
    
    filename = "#{radix_head}%04d#{radix_tail}" % weight

    File.open(filename, 'w') do |output|
      output_event_file.write_to(output)      
    end
  end
end

###
###  MAIN
###

options = {:split => nil}
option_parser = OptionParser.new do |opts|
  opts.banner = "Usage: #{File.basename($0)} [options] filter"

  opts.on("-sRADIX", "--split=RADIX", "Split by weight. %w is replaced by the weight. By default _%w is appended to the basename") do |radix|
    options[:split] = radix
  end

  opts.on("-h", "--help", "Prints this help") do
    puts opts
    exit
  end
end

begin 
  option_parser.parse!
rescue OptionParser::ParseError => err
  puts(err)
  puts(option_parser)
  exit(1)
end



input  = $stdin
output = $stdout
log    = $stderr


die if ARGV.size != 1

filter = load_filter(ARGV.shift)


event_data = load_event_file(input, log)

output_event_file = Event_File.new(event_data)
output_event_file.metadata['polarity']='float'

frames = collect_by_timestamp(event_data);

frames.each do |frame|
  new_events = process_frame(frame, filter)
  output_event_file.events.concat(new_events)  
end


output_event_file.metadata['max_polarity'] =
  output_event_file.events.map {|ev| ev.polarity.abs}.max

if options[:split].nil?
  output_event_file.write_to(output)
else
  split_by_weight(output_event_file, options[:split])
end

