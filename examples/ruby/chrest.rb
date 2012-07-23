# Helper routines for working with CHREST from jRuby
# -- perhaps convert into a RubyGem

require "java"
require "../../chrest"

include_class [
  "jchrest.architecture.Chrest", 
  "jchrest.gui.ChrestView", 
  "jchrest.lib.ChessDomain",
  "jchrest.lib.Pattern",
  "jchrest.lib.Scene",
  "jchrest.lib.Scenes"
]

module ChrestPattern
  def ChrestPattern.make_pattern_from_string str
    Pattern.makeVisualList(str.split("").to_java(:String))
  end

  def ChrestPattern.make_verbal_pattern_from_string str
    Pattern.makeVerbalList(str.split("").to_java(:String))
  end

  def ChrestPattern.makeSRPairs pairs
    pairs.collect {|s, r| [ChrestPattern.make_pattern_from_string(s), ChrestPattern.make_pattern_from_string(r)]}
  end
end

class Scenes
  # Read in a set of Scene descriptions
  def Scenes.read_from_file filename
    scenes = []
    begin 
      fbr = java.io.BufferedReader.new(java.io.FileReader.new(filename))
      fbr.read_line # reads and ignores the 'visual search' line
      scenes = Scenes.read(fbr)
    rescue java.io.IOException 
      puts "Error !"
    end
    return scenes
  end
end

class Chrest
  # Create a model, training on each scene n times
  def Chrest.create_model(scenes, cycles, fixations = 20, cap = 1_000_000)
    model = Chrest.new
    model.set_domain(ChessDomain.new)
    puts "Learning: "
    puts "Cycle  Visual LTM size  Avg depth  Avg image size"
    cycles.times do |cycle|
      break if model.ltm_visual_size > cap 
      scenes.size.times do |i|
        break if model.ltm_visual_size > cap 
        model.learn_scene(scenes.get(i), fixations)
      end
      puts "#{"%5d" % (cycle+1)}  #{"%15d" % model.ltm_visual_size}  #{"%9.2f" % model.get_visual_ltm_average_depth}  #{"%14.2f" % model.get_visual_ltm_average_image_size}"
    end
    model
  end
end

class Scene
  # Display this scene onto standard output
  def display
    height.times do |h|
      width.times do |w|
        print get_item(h, w)
      end
      puts
    end
  end

end
