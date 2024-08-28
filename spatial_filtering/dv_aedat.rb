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
      accum = (accum << 2) + (ev.polarity.value == 1 ? On : Off)
      accum = accum << 10

      return accum
    end
    
    def self.write(output, events)
      raise StandardError unless
        output.is_a?(IO) &&
        events.is_a?(Event_Sequence)

      output.print(Header)
      output.print("# AEChip: eu.seebetter.ini.chips.davis.Davis640\r\n")

      events.each do |ev|
        output.write([coordinates(ev)].pack("N"))
        output.write([ev.timestamp.value].pack("N"))

        # puts "#{ev.timestamp.value}/#{[ev.timestamp.value].pack("N")}/"
      end

    end # def write

  end # IO_AEDAT
end # DV
