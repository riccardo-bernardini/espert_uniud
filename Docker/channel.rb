require 'socket'
require 'definitions'
require 'logger'

Socket_Name = File.join(Tree[:socket], 'worker')

class Server_Side
  @@logger = nil
  
  @@listening_socket = nil
  
  def initialize
    if @@logger.nil?
      @@logger=Logger.new(Tree.join(:log, 'channel-server.log'))
    end
      
    if @@listening_socket.nil?
      File.unlink(Socket_Name) if File.exists?(Socket_Name)

      @@logger.info('Opening listening socket')
      @@listening_socket=UNIXServer.new(Socket_Name)
      @@logger.info(File.stat(Socket_Name).mode.to_s(8))
      File.chmod(0777, Socket_Name)

      @@logger.info('Done')
    end

    @@logger.info('Waiting for clients')

    @to_client = @@listening_socket.accept

    @@logger.info('Client connected')
    
    if block_given?
      @@logger.info('Yielding')

      yield(self)
      @to_client.close
      
      @@logger.info('Done')
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
  @@logger = Logger.new(Tree.join(:log, 'channel-client.log'))
  
  def initialize
    timeout = 60

    @@logger.info("Checking for socket at #{Socket_Name}")

    while ! File.exists?(Socket_Name) do
      @@logger.info("Not there. Sleeping")
      
      sleep(1)
      timeout = timeout - 1

      raise "Cannot find server socket" if timeout == 0
    end

    @@logger.info("Got it")
    
    @to_server = UNIXSocket.new(Socket_Name)

    @@logger.info("Connected to server")

    if block_given?
      @@logger.info("Yielding")

      yield(self)
      @to_server.close

      @@logger.info("Done")
    end
  end

  def puts(x)
    @to_server.puts(x)
  end

  def close
    @to_server.close
  end
end
