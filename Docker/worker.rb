#!/usr/bin/env ruby

$my_dir = File.dirname(File.absolute_path(__FILE__))
$LOAD_PATH.unshift(File.join($my_dir, "lib"))

require 'open3'
require 'zip'

require 'channel'


Accumulator_Path = File.join($my_dir, "../exe/accumulator.exe");

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

loop do
  connection=Server_Side.new

  params = connection.readlines.map {|s| s.chomp}

  connection.close

  stderr_file        = check(params.shift, "stderr")
  stdout_file        = check(params.shift, "stdout")
  status_file        = check(params.shift, "status")
  image_glob_pattern = check(params.shift, "images")
  zip_filename       = check(params.shift, "zip")


  stdout, stderr, status=Open3.capture3(Accumulator_Path, *params);

  File.write(stdout_file, stdout);
  File.write(stderr_file, stderr);

  if status.success?
    File.write(status_file, Accumulator_Done);

    if create_zip_archive(zip_filename, image_glob_pattern)
      File.write(status_file, Archive_Ready)
    else
      File.write(status_file, Error_while_Zipping)
    end
  else
    File.write(status_file, exitcode_to_status(status.exitstatus));
  end
end
