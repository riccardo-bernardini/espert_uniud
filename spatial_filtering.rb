#!/usr/bin/env ruby

Struct.new('Pair', 'x', 'y');


class Event
  def initialize(timestamp, x, y, polarity)
    raise "Bad timestamp" unless timestamp.is_a?(String)
    
    @timestamp=timestamp
    @x=x.to_i
    @y=y.to_i

    if polarity.is_a?(String)
      @polarity= if polarity=='1'
                   1
                 else
                   -1
                 end
    else
      @polarity = polarity
    end
  end  

  def to_s
    return "#{@timestamp},#{@x},#{@y},#{@polarity}"
  end

  attr_reader :timestamp, :x, :y, :polarity
end

class Image
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
    @pixels.each do |key, val|
      yield(key.x, key.y, val)
    end
  end
end

class CSV_Line
  def initialize(line)
    @content = line

    case line
    when /^ *#/
      @linetype = :comment

    when /^[a-zA-Z, ]+$/
      @linetype = :header

    when /^[0-9, ]+$/
      @linetype = :event

      fields = line.split(',')

      if fields.size != 4
        @linetype = :bad
        return
      end
      
      @event = Event.new(fields[0], fields[1], fields[2], fields[3])
    else
      @linetype = :bad

    end
  end

  def to_s
    case @linetype
    when :comment
      return @content

    when :header
      return @content

    when :bad
      return "Badly formed line '#{@content}'"

    when :event
      return @event.to_s

    else
      raise "??? #{@linetype}"
    end
  end
  
  attr_reader :event, :linetype, :content
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
  # spec
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

def parse_line(input)
  line = input.gets

  return nil if line.nil?
  
  return CSV_Line.new(line.chomp)
end
  
def process_frame(frame, filter)
  t0 = frame[0].timestamp
  accumulator = Image.new

  frame.each do |event|
    raise "Bad, bad, bad" unless t0 == event.timestamp
    
    filter.each do |delta_x, delta_y, value|
      accumulator[event.x+delta_x, event.y+delta_y] += event.polarity * value
    end
  end

  return accumulator.map do |x, y, val|
    Event.new(t0, x, y, val)
  end
end
  

die if ARGV.size != 1

filter = load_filter(ARGV.shift)

input=$stdin
output=$stdout
log=$stderr

current_timestamp = -1
current_frame = []

header_seen = false

while line = parse_line(input)
  case line.linetype
  when :comment
    output.puts(line)

  when :header
    if ! header_seen
      output.puts(line)
      header_seen = true

    else
      log.puts("Duplicated header #{line.content}")
      
    end
  when :bad
    log.puts("Badly formed line  '#{line.content}'")
    
  when :event
    event = line.event
    if event.timestamp == current_timestamp
      current_frame << event
    else
      unless current_frame.empty?
        new_events = process_frame(current_frame, filter)
        new_events.each {|ev| output.puts(ev)}
      end
      
      current_frame = [ event ];
      timestamp = event.timestamp
    end
  end
end

unless current_frame.empty?
  new_events = process_frame(current_frame, filter)
  new_events.each {|ev| output.puts(ev)}
end



