#!/usr/bin/env ruby

require 'tk'

load 'event_files.rb'

def die(msg=nil)
  if msg
    $stderr.puts(msg)
  else
    $stderr.puts "Usage: #{$0} event-file"
  end
  
  exit(1)
end

def polarity_to_color(polarity)
  if polarity > 0
    'blue'
  else
    'red'
  end
end

def update_canvas
  $canvas.delete(:all)

  $events[$time_index].each do |event|
    color = polarity_to_color(event.polarity)
    TkcRectangle.new($canvas, event.x, event.y, event.x+$dx, event.y+$dy) {
      outline color
      fill color
    }
  end
end

def update_index(key)
  case key
  when "Left"
    $time_index -= 1 unless $time_index == 0

  when "Right"
    $time_index += 1 unless $time_index == $events.size - 1

  end
end

die if ARGV.size != 1

event_file=nil

File.open(ARGV.shift) do |input|
  event_file = load_event_file(input)
end

$dx=2
$dy=2

$events = collect_by_timestamp(event_file)

$root = TkRoot.new

$canvas = TkCanvas.new ($root) do
  height event_file.metadata['sizeY']
  width  event_file.metadata['sizeX']
  pack
end

$root.bind('KeyPress', proc {|k|  update_index(k); update_canvas }, "%K")

$canvas_items = []

$time_index = 0

update_canvas

Tk.mainloop

