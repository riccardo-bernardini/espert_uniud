class Event
  def initialize(timestamp, x, y, polarity, polarity_type)
    @timestamp=timestamp.to_i
    @x=x.to_i
    @y=y.to_i

    @polarity_type=polarity_type

    @polarity = case @polarity_type
                when 'boolean'
                  if polarity.to_i == 1
                    1
                  else
                    -1
                  end

                when 'float'
                  polarity.to_i

                end

    if false && polarity_type == 'boolean'
      $stderr.puts("[#{polarity},#{@polarity},#{@polarity_type}]")
    end
  end  

  def to_s
    polarity = case @polarity_type
               when 'boolean'
                 if @polarity == 1
                   '1'
                 else
                   '0'
                 end

               when 'float'
                 @polarity
               end
    
    return "#{@timestamp},#{@x},#{@y},#{polarity}"
  end

  attr_reader :timestamp, :x, :y, :polarity
end

def parse_metadata(line)
  metadata = Hash.new

  while line =~ /^ *([a-zA-Z]+) *: *([^ ]+)/
    key = $1
    value = $2
    line = $'

    metadata[key]=value
  end

  return metadata
end

class CSV_Line
  def initialize(line, polarity_type)
    @content = line

    case line
    when /^ *#/
      @linetype = :comment
      @metadata = parse_metadata(line[1..-1])

    when /^[a-zA-Z, ]+$/
      @linetype = :header

    when /^[0-9]+ *, *[0-9]+ *, *[0-9]+ *, *[-0-9]+ *$/
      @linetype = :event

      fields = line.split(',')

      if fields.size != 4
        @linetype = :bad
        return
      end
      
      @event = Event.new(fields[0], fields[1], fields[2], fields[3], polarity_type)
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
  
  attr_reader :event, :linetype, :content, :metadata
end

def parse_line(input, polarity_type)
  line = input.gets

  return nil if line.nil?
  
  return CSV_Line.new(line.chomp, polarity_type)
end

class Event_File
  def initialize(parent_data = nil)
    @events = Array.new
    @header  = nil
    @metadata = {'polarity' => 'boolean'}

    unless parent_data.nil?
      @metadata.update (parent_data.metadata)
      @header = parent_data.header
    end
  end

  def write_to(stream)
    meta_line = '# ' + @metadata.map {|key, value| "#{key}: #{value}"}.join(' ')
    stream.puts(meta_line)

    stream.puts(@header) if @header

    @events.each do |event|
      stream.puts(event.to_s)
    end
  end

  def header=(s)
    @header=s
  end

  attr_reader :events, :metadata, :header
end

def load_event_file(input, log=$stderr)
  event_file = Event_File.new
  header_seen = false

  while line = parse_line(input, event_file.metadata['polarity'])
    case line.linetype
    when :comment
      event_file.metadata.update(line.metadata)

    when :header
      if ! header_seen
        event_file.header = line
        header_seen = true

      else
        log.puts("Duplicated header #{line.content}")
      
      end
    when :bad
      log.puts("Badly formed line  '#{line.content}'")
    
    when :event
      event_file.events << line.event
    end
  end

  return event_file
end


def collect_by_timestamp(event_data)
  collection = [];
  
  current_timestamp = 0
  current_frame = []

  event_data.events.each do |event|
    if current_timestamp == event.timestamp
      current_frame << event

    elsif event.timestamp > current_timestamp
      collection << current_frame unless current_frame.empty?
      current_frame = [ event ]
      current_timestamp = event.timestamp

    else
      raise "Bum! #{current_timestamp} #{event.timestamp}"
      
    end
  end

  collection << current_frame unless current_frame.empty?

  return collection
end
