require 'socket'
require 'definitions'
require 'logger'

require 'socket_name'

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

