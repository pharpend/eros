#!/usr/bin/env ruby

require 'json'
require 'pp'

filePaths = ARGV
filePaths.each do |filePath| 
  fileHandle = File::open filePath, "r"
  fileText = fileHandle.read
  fileHandle.close

  fileParsed = JSON::parse fileText

  def fixForest(phraseScoreForest)
    return {} if phraseScoreForest.nil?
    newForest = phraseScoreForest["forest"].map do |forest| 
      betterForest = forest[0]
      fixForest(betterForest)
    end
    phraseScoreForest["forest"] = newForest
    phraseScoreForest
  end
  fixedForests = fileParsed.map{ |psf| fixForest(psf) }
  fixedJSON = JSON::pretty_generate fixedForests

  fileHandle = File::open(filePath, 'w')
  fileHandle.write(fixedJSON)
  fileHandle.close
end 
