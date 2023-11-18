#!/usr/bin/env ruby

require 'socket'
require 'open3'
require 'zip'

socket_name = '/tmp/aaa'

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


File.unlink(socket_name) if File.exists?(socket_name)

server=UNIXSocket.new(socket_name)

loop do
  connection=server.accept

  params = connection.readlines.map {|s| s.chomp}

  connection.close

  stderr_file=params.shift
  stdout_file=params.shift
  status_file=params.shift
  zip_filename=params.shift
  image_dir=params.shift

  stdout, stderr, status=Open3.capture3(Accumulator_Path, *params);

  File.write(stdout_file, stdout);
  File.write(stderr_file, stderr);

  if status.success?
    File.write(status_file, Accumulator_Done);

    if create_zip_archive(zip_filename, File.join(image_dir, '*.png'))
      File.write(status_file, Archive_Ready)
    else
      File.write(status_file, Error_while_Zipping)
    end
  else
    File.write(status_file, exitcode_to_status(status.exitstatus));
  end
end
