#!/usr/bin/ruby

require 'json'

text = JSON::parse File::read "phraselist-ugly.json"

text.each do |name, tree|
  f = File.open("phraselists/#{name}.json", "w")
  f.write(JSON::pretty_generate(tree))
  f.close()
end
