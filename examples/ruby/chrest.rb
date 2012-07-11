# Helper routines for working with CHREST from jRuby
# -- perhaps convert into a RubyGem

require "java"
require "../../chrest"

include_class [
  "jchrest.architecture.Chrest", 
  "jchrest.gui.ChrestView", 
  "jchrest.lib.Pattern"
]

module ChrestPattern
  def ChrestPattern.make_pattern_from_string str
    Pattern.makeVisualList(str.split("").to_java(:String))
  end

  def ChrestPattern.makeSRPairs pairs
    pairs.collect {|s, r| [ChrestPattern.make_pattern_from_string(s), ChrestPattern.make_pattern_from_string(r)]}
  end
end
