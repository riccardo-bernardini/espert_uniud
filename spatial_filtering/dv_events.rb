
module DV
  #
  # This module contains resources to work with the files of
  # DV cameras.
  #
  class Wrapper
    def to_s
      @value.to_s
    end
  end
  
  class Timestamp < Wrapper
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

  class Coordinate < Wrapper
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

  class Polarity
    #
    #  Polarity of a DV event. Just a wrapper around an integer
    #  to do a kind of type check.
    #

    def initialize(polarity, polarity_type)

      raise StandardError unless
        [:boolean, :float].include?(polarity_type) &&
        polarity.is_a?(Integer)

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

    def to_s
      case @polarity_type
      when :boolean
        @polarity == 1 ? 1 : 0

      when :float
        @polarity

      else
        raise StandardError.new("I should not be here")
      end
    end # def to_s
    
    attr_reader :value
    attr_reader :polarity_type
  end

  class Event
    #
    #  Event of a DV camera.  It is characterized by the time
    #  it happened, where it happened and its sign
    #
    def initialize(timestamp, x, y, polarity)
      
      raise StandardError unless
        timestamp.is_a?(DV::Timestamp) &&
        x.is_a?(DV::Coordinate) &&
        y.is_a?(DV::Coordinate) &&
        polarity.is_a?(DV::Polarity)
      
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


  class Event_Sequence
    def initialize
      @events = Array.new
    end

    def <<(x)
      raise StandardError unless x.is_a?(DV::Event)
      
      @events << x
    end

    def each
      @events.each {|ev| yield(ev)}
    end
  end



  def self.collect_by_timestamp(event_data)
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


  def self.collect_by_weight(event_data)
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


end # module DV

