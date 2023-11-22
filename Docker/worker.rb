#!/usr/bin/env ruby

$my_dir = File.dirname(File.absolute_path(__FILE__))
$LOAD_PATH.unshift(File.join($my_dir))

require 'open3'
require 'zip'

require 'channel'
require 'definitions'


Accumulator_Path = File.join($my_dir, "accumulator");

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
  
  $stderr.puts("Waiting...") if $verbose

  Server_Side.new do |connection|
    $stderr.puts("Connected. Reading...") if $verbose
    params = connection.readlines.map {|s| s.chomp}
  end

  $stderr.puts("Done") if $verbose

  stderr_file        = check(params.shift, "stderr")
  stdout_file        = check(params.shift, "stdout")
  status_file        = check(params.shift, "status")
  image_glob_pattern = check(params.shift, "images")
  zip_filename       = check(params.shift, "zip")

  $stderr.puts("Calling accumulator...") if $verbose
  stdout, stderr, status=Open3.capture3(Accumulator_Path, *params);

  $stderr.puts("Done") if $verbose
  
  File.write(stdout_file, stdout);
  File.write(stderr_file, stderr);

  if status.success?
    File.write(status_file, Accumulator_Done);

    $stderr.puts("Creating zip...") if $verbose

    if create_zip_archive(image_glob_pattern, zip_filename)
      $stderr.puts("Zip ready") if $verbose
      File.write(status_file, Archive_Ready)
    else
      File.write(status_file, Error_while_Zipping)
    end
  else
    $stderr.puts("Accumulator error #{status.exitstatus}");
    File.write(status_file, exitcode_to_status(status.exitstatus));
  end
end
