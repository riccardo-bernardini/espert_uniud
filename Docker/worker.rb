#!/usr/bin/env ruby

require 'socket'

socket_name = '/tmp/aaa'

File.unlink(socket_name) if File.exists?(socket_name)

server=UNIXSocket.new(socket_name)

loop do
  connection=server.accept

  params = connection.readlines

  connection.close

  stderr_file=params.shift
  stdout_file=params.shift
  image_dir=params.shift
end
