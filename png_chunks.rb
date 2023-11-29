#!/usr/bin/env ruby

def take(len, data)
  return data[0...len], data[len..-1]
end
data = File.read(ARGV.shift)

def leggi(data)
  junk, data = take(8, data)

  chunks = Array.new
  while data.size > 0
    n=data.size
    h, data = take(4, data)
    puts "#{h.size+data.size}, #{n}"
    len=h.unpack('N')

    puts "#{h.unpack('C*')}"
    len = len[0]

    n=data.size
    chunk, data = take(4, data)
    puts "#{chunk.size+data.size}, #{n}"
    
    puts "#{chunk} (#{len}) #{data.size}"

    n=data.size
    body, data = take(len, data)

    if data.nil?
      puts "STOP body"
      return
    end

    puts "#{body.size+data.size}, #{n}"

    n=data.size
    crc, data = take(4, data)

    if data.nil?
      puts "STOP crc"
      return
    end

    puts "#{crc.size+data.size}, #{n}"
  end
end

def leggi2(data)
  data = data.unpack('C*').map {|x| (x <32 || x > 127) ? 32 : x }.pack('C*')
  while data =~ /[A-Za-z][A-Za-z][A-Z][A-Za-z]/
    chunk=Regexp.last_match[0]
    data=Regexp.last_match.post_match

    puts chunk
  end
end

leggi(data)
