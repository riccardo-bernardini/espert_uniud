require 'socket'
require 'definitions'
require 'logger'

require 'socket_name'

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
