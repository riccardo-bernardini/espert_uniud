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
    $stderr.puts "Usage: #{$0} filter"
    $stderr.puts "Read stdin, write stdout"
  end
  
  exit(1)
end

def load_filter_from_file(filename)
  die("Loading from file '#{filename}' not implemented");
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

###
###  MAIN
###

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

output_event_file.write_to(output)

