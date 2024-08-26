
module DV
  #
  # This module contains resources to work with the files of
  # DV cameras.
  #
  class DV.Timestamp
    #
    #  Timestamp of a DV event. Just a wrapper around an integer
    #  to do a kind of type check.
    #
    def initialize(x)
      raise StandardError unless x.is_a? Integer
      
      @value=x;
    end

    attr_reader :value
  end

  class DV.Coordinate
    #
    #  Coordinate of a DV event. Just a wrapper around an integer
    #  to do a kind of type check.
    #
    def initialize(x)
      raise StandardError unless x.is_a? Integer
      
      @value=x;
    end

    attr_reader :value
  end

  class DV.Polarity
    #
    #  Polarity of a DV event. Just a wrapper around an integer
    #  to do a kind of type check.
    #

    def initialize(polarity, polarity_type)

      raise StandardError unless
        [:boolean, :float].include?(polarity_type) &&
        polarity.is_a? Integer 

      @polarity_type=polarity_type

      @polarity = case @polarity_type
                  when :boolean
                    polarity == 1 ? 1 : -1

                  when :float
                    polarity

                  else
                    raise StandardError.new("I should not be here")
                  end

    end

    attr_reader :value
    attr_reader :polarity_type
  end

  class DV.Event
    #
    #  Event of a DV camera.  It is characterized by the time
    #  it happened, where it happened and its sign
    #
    def initialize(timestamp, x, y, polarity)
      
      raise StandardError unless
        timestamp.is_a?(DV.Timestamp) &&
        x.is_a?(DV.Coordinate) &&
        y.is_a?(DV.Coordinate) &&
        polarity.is_a?(DV.Polarity)
      
      @timestamp=timestamp
      @x=x
      @y=y
      @polarity=polarity
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
    attr_writer :polarity
  end


  class DV.Event_Line
    def initialize(line, polarity_type)
      @content = line

      case line
      when /^ *#(.*)$/
        @linetype = :comment
        @metadata = parse_metadata($1)

      when /^[a-zA-Z, ]+$/
        @linetype = :header

      when /^[0-9]+ *, *[0-9]+ *, *[0-9]+ *, *[-0-9]+ *$/
        @linetype = :event

        fields = line.split(',')

        if fields.size != 4
          @linetype = :bad
          return
        end

        fields.each do |value|
          unless value =~ /^ *[0-9]+ *$/
            @linetype = :bad
            break
          end
        end

        if @linetype == :event
          @event = DV.Event.new(DV.Timestamp.new(fields[0].to_i),
                                DV.Coordinate.new(fields[1].to_i),
                                DV.Coordinate.new(fields[2].to_i),
                                DV.Polarity_Type(fields[3].to_i, polarity_type))

        elsif @linetype == :bad
        # Nothing to do
        else
          raise StandardError.new("I shouldn't be here")
        end
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

    private

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

  end


  class DV.Event_File
    def initialize(parent_data = nil)
      @events = Array.new
      @header  = nil
      @metadata = {'polarity' => 'boolean'}
      @errors = []
      
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

    def error_error(err)
      @errors << err
    end

    def clear_errors
      @errors = Array.new
    end

    def <<(x)
      raise StandardError unless x.is_a? DV.Event
      
      @events << x
    end

    def header=(s)
      @header=s
    end

    attr_reader :events, :metadata, :header, :errors
  end

  def DV.load_event_file(input)
    def DV.parse_line(input, polarity_type)
      line = input.gets
      
      return nil if line.nil?
    
      return DV.Event_Line.new(line.chomp, polarity_type)
    end

    event_file = DV.Event_File.new
    header_seen = false

    while line = parse_line(input, event_file.metadata['polarity'])

      raise StandardError unless line.is_a?(DV.Event_Line)
      
      case line.linetype
      when :comment
        event_file.metadata.update(line.metadata)

      when :header
        if ! header_seen
          event_file.header = line
          header_seen = true

        else
          event_file.errors << "Duplicated header #{line.content}"
          
        end
      when :bad
        event_file.errors << "Badly formed line  '#{line.content}'"
        
      when :event
        event_file << line.event
      end
    end

    return event_file
  end


  def DV.collect_by_timestamp(event_data)
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


  def DV.collect_by_weight(event_data)
    result = Hash.new

    t0 = event_data.events[0].timestamp

    event_data.events.each do |event|
      if result[event.polarity].nil?
        result[event.polarity] = [ Event.new(t0, 0, 0, 1, 'boolean') ]
      end

      result[event.polarity] << event
    end

    return result
  end
end
