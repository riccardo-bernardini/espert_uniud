#!/usr/bin/env ruby


require 'logger'
require 'open3'
require 'zip'
require 'channel'
require 'definitions'

$logger=Logger.new(STDERR)

Accumulator_Path = Tree.join(:bin, "accumulator");

def create_zip_archive(pattern, zipfile_name)
  input_filenames = Dir.glob(pattern);

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

def check(value, label)
  head, body = value.split(':', 2)

  raise "This shouldn't happen" unless head == label

  return body
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

  stderr_file        = check(params.shift, "stderr")
  stdout_file        = check(params.shift, "stdout")
  status_file        = check(params.shift, "status")
  image_glob_pattern = check(params.shift, "images")
  zip_filename       = check(params.shift, "zip")

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
