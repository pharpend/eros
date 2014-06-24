#!/usr/bin/env ruby

require 'json'
require 'pp'

def deleteEmpty(data)
  data2 = data.reject { |x| x.empty? }
  data3 = data2.map do |d| 
    d["forest"] = deleteEmpty(d["forest"])
    d
  end
  data3
end

filePaths = ARGV
filePaths.each do |filePath| 
  fileText = File::read filePath
  fileParsed = JSON::parse fileText

  fixedData = deleteEmpty(fileParsed)
  fixedJson = JSON::pretty_generate fixedData

  fileHandle = File::open filePath, "w"
  fileHandle.write fixedJson
  fileHandle.close
end 
