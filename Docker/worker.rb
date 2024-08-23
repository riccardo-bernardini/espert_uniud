#!/usr/bin/env -S ruby -I /usr/local/apache2/library

#
# This script Ruby acts as a bridge between frame_maker (called
# as CGI when the user submit the data via index.html) and
# `accumulator`, the actual program that converts events into
# frames.
#
# This program is launched by worker-manager.sh (which relaunches it
# if worker.rb crashes) and enter an infinite loop where it waits for
# a connection by frame_maker via an internal socket.
#
# When the connection happens, it reads from the socket the
# parameters passed by frame_maker and then it calls accumulator.
# When accumulator ends its job, it collects the generated frames
# into a zip file whose name was provided by frame_maker.
#
# After creating the zip file, the loop restarts and this scripts
# waits again for a new connection.
#
# When the zip file is ready JavaScript code in the HTML page sent
# back to the user by frame_maker will enable the download button.
#

#
# This script is run by a script with root identity.  For
# security, it is better to change user ID to www-data
#
Username = 'www-data'
User_UID = Process::UID.from_name(Username)

Process::UID.change_privilege(User_UID)

require 'definitions'

require 'logger'
require 'open3'
require 'zip'
require 'channel_server'

$logger=Logger.new(STDERR)

Accumulator_Path = Tree.join(:bin, "accumulator");

def create_zip_archive(pattern, zipfile_name)
  input_filenames = Dir.glob(pattern);

  $logger.info("zip pattern=#{pattern};")

  $logger.info("filenames=#{input_filenames.inspect};")

  return nil if input_filenames.empty?


  Zip::File.open(zipfile_name, Zip::File::CREATE) do |zipfile|
    input_filenames.each do |path|
      # Two arguments:
      # - The name of the file as it will appear in the archive
      # - The original file, including the path to find it
      zipfile.add(File.basename(path), path)
    end
  end

  return zipfile_name
end

def extract(parameters, label)
  #
  # The parameters passed by frame_maker can be split into two
  # classes
  #
  # 1. Parameters interpreted by accumulator. They have the format
  #    of an option (e.g., --sampling=1ms) and they are give as
  #    they are to accumulator
  #
  # 2. Private parameters, that is, Parameters interpreted by this 
  #    script.  They have the format label:value.
  #
  # This function looks into parameters (which holds the lines wrote
  # by frame_maker) and searches for a private parameter with the
  # given label and returns the corresponding value.  The found
  # entry is removed from parameters.  It is an error if label is
  # not found.
  #
  idx = parameters.find_index {|x| x.start_with?("#{label}:") }

  raise "This shouldn't happen" if idx.nil?
  
  key, value = parameters[idx].split(':', 2)

  raise "This shouldn't happen" unless label==key

  parameters.delete_at(idx)

  # $logger.info(parameters.inspect)
  
  return value
end

def is_external_parameter?(x)
  x.start_with?("--")
end

def has_only_external_parameters?(parameters)
  return parameters.all? {|x| is_external_parameter?(x)}
end

$verbose=true

loop do
  params = Array.new
  
  $logger.info("Waiting...") 

  Server_Side.new do |connection|
    $stderr.puts("Connected. Reading...") if $verbose
    params = connection.readlines.map {|s| s.chomp}
  end

  $logger.info("Done") 

  stderr_file        = extract(params, "stderr")
  stdout_file        = extract(params, "stdout")
  status_file        = extract(params, "status")
  image_glob_pattern = extract(params, "images")
  zip_filename       = extract(params, "zip")

  $logger.info(params.inspect)

  #
  # Here I should have removed all the private parameters and
  # params should contain only external parameters
  #
  if ! has_only_external_parameters?(params)
    raise "Unexpected private parameters in #{params} (this should not happen)"
  end

  $logger.info("Calling accumulator...") 
  stdout, stderr, status=Open3.capture3(Accumulator_Path, *params);

  $logger.info("Done")
  
  File.write(stdout_file, stdout);
  File.write(stderr_file, stderr);

  if status.success?
    File.write(status_file, Accumulator_Done);

    $logger.info("Creating zip...") 

    if create_zip_archive(image_glob_pattern, zip_filename)
      $logger.info("Zip ready") 
      File.write(status_file, Archive_Ready)
    else
      $logger.error("Could not create zip file")
      File.write(status_file, Error_while_Zipping)
    end
  else
    $logger.error("Accumulator error #{status.exitstatus}");
    $logger.error("Accumulator stderr begin-----------------------------");
    $logger.error(stderr)
    $logger.error("Accumulator stderr end-----------------------------");

    File.write(status_file, exitcode_to_status(status.exitstatus));
  end
end
