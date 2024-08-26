#require "dv_events"

module DV
  module IO_AEDAT_2

    Header = "#!AER-DAT2.0\r\n"
    On = 2
    Off = 0
    
    def self.type_bit(format)
      case format
      when :dvs
        0

      when :aps
        1

      else
        raise StandardError
      end
    end

    def self.coordinates(ev)
      raise StandardError unless ev.is_a?(DV::Event)

      accum = type_bit(:dvs)
      accum = (accum << 9) + ev.y.value
      accum = (accum << 10) + ev.x.value
      accum = (accum << 2) + (ev.polarity == 1 ? On : Off)
      accum = accum << 10

      return accum
    end
    
    def self.write(output, events)
      raise StandardError unless
        output.is_a?(IO) &&
        events.is_a?(Event_Sequence)

      output.print(Header)

      events.each do |ev|
        packet = (coordinates(ev) << 32) + ev.timestamp.value
        output.write([packet].pack("Q"))
      end

    end # def write

  end # IO_AEDAT
end # DV
