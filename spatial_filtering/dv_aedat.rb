#require "dv_events"

module DV
  module DV::IO_AEDAT_2

    DV::IO_AEDAT_2::Header = "#!AER-DAT2.0\r\n"
    DV::IO_AEDAT_2::On = 2
    DV::IO_AEDAT_2::Off = 0
    
    def DV::IO_AEDAT_2::type_bit(format)
      case format
      when :dvs
        0

      when :aps
        1

      else
        raise StandardError
      end
    end

    def DV::IO_AEDAT_2::coordinates(ev)
      raise StandardError unless ev.is_a?(DV::Event)

      accum = DV::IO_AEDAT_2::type_bit(:dvs)
      accum = (accum << 9) + ev.y
      accum = (accum << 10) + ev.x
      accum = (accum << 2) + (ev.polarity == 1 ? DV::IO_AEDAT_2::On : DV::IO_AEDAT_2::Off)
      accum = accum << 10

      return accum
    end
    
    def DV::IO_AEDAT_2::write(output, events)
      raise StandardError unless
        output.is_a?(IO) &&
        events.is_a?(DV::Event_Sequence)

      output.print(DV::IO_AEDAT_2::Header)

      events.each do |ev|
        output.write(DV::IO_AEDAT_2::coordinates(ev).pack("L"))
        output.write(ev.timestamp.pack("L"))
      end

    end # def save

  end # IO_AEDAT
end # DV
