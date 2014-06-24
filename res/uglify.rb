#!/usr/bin/env ruby

require 'json'

filePaths = ARGV
filePaths.each do |prettyPath|
  fileText = File::read prettyPath
  jsonPretty = JSON::parse fileText
  jsonUgly = JSON::generate jsonPretty
  uglyPath = prettyPath.gsub "pretty", "ugly"
  uglyHandle = File::open uglyPath, 'w'
  uglyHandle.write jsonUgly
  uglyHandle.close
end
