require "dv_events"

module DV
  module DV::IO_CSV
    class DV::IO_CSV::Metadata
      Metadata_Conversions = {'polarity' => :to_sym }

      def initialize
        @table = {'polarity' => :boolean}
      end

      def each
        @table.each {|a, b| yield(a,b)}
      end

      def update(other)
        other.each {|key,val| @table[key]=val}
      end

      def [](key)
        @table[key]
      end

      def []=(key,val)
        if Metadata_Conversions[key]
           val=val.send(Metadata_Conversions[key])
        end
        
        @table[key]=val
      end
      
      def polarity
        @table['polarity']
      end

      def header
        @table[:header]
      end
      
      def header=(x)
        @table[:header]=x
      end
    end
    
    class DV::IO_CSV::Event_Line
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
        metadata = DV:IO_CSV::Metadata.new

        while line =~ /^ *([a-zA-Z]+) *: *([^ ]+)/
          key = $1.downcase
          value = $2
          line = $'

          metadata[key]=value
        end

        return metadata
      end
    end # class Event_Line


    def DV::IO_CSV::load(input)

      event_file = DV.Event_Sequence.new
      metadata = DV::IO_CSV::Metadata.new
      errors = Array.new
      
      while line = input.gets
        line = DV::IO_CSV::Event_Line.new(line.chomp,
                                          metadata.polarity)
        
        raise StandardError unless line.is_a?(DV::IO_CSV::Event_Line)
        
        case line.linetype
        when :comment
          metadata.update(line.metadata)

        when :header
          if metadata.header.nil?
            metadata.header = line

          else
            errors << "Duplicated header #{line.content}"
            
          end
        when :event
          event_file << line.event

        when :bad
          errors << "Badly formed line  '#{line.content}'"

        else
          raise StandardError.new("I shouldn't be here")
          
        end
      end

      return [event_file, errors, metadata]
    end # def load

    def DV::IO_CSV::save(output, events, metadata=DV::IO_CSV::Metadata.new)
      raise StandardError unless
        (output.is_a?(String) || output.is_a?(IO)) &&
        events.is_a?(DV::Event_Sequence) &&
        metadata.is_a?(DV::IO_CSV::Metadata)

      if output.is_a?(String)
        File.open(output, 'w') do |stream|
          DV::IO_CSV::save(stream, events, metadata)
        end

      else
        # meta_line = '# ' + @metadata.map {|key, value| "#{key}: #{value}"}.join(' ')
        # stream.puts(meta_line)

        stream.puts(metadata.header) if metadata.header

        events.each do |ev|
          stream.puts("#{ev.timestamp},#{ev.x},#{ev.y},#{ev.polarity}")
        end # each
      end # if
    end # def save
  end # module IO_CSV
end # module DV
