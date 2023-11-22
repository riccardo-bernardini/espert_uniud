require 'socket'
require 'definitions'

def socket_dir
  return File.dirname(File.absolute_path(__FILE__))
end

Socket_Name = File.join(socket_dir, worker)

class Server_Side
  @@listening_socket = nil
  
  def initialize
    if @@listening_socket.nil?
      File.unlink(Socket_Name) if File.exists?(Socket_Name)

      @@listening_socket=UNIXServer.new(Socket_Name)
    end

    @to_client = @@listening_socket.accept

    if block_given?
      yield(self)
      @to_client.close
    end
  end

  def readlines
    @to_client.readlines
  end

  def close
    @to_client.close
  end
end

class Client_Side
  def initialize
    @to_server = UNIXSocket.new(Socket_Name)

    if block_given?
      yield(self)
      @to_server.close
    end
  end

  def puts(x)
    @to_server.puts(x)
  end

  def close
    @to_server.close
  end
end
