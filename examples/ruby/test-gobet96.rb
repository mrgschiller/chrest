# Experiment on chess expertise as given in de Groot and Gobet (1996)
# Explores difference in master-novice behaviours at:
#   1. speed of eye fixations
#   2. coverage of board
#   3. relevance of squares covered
#   4. recall performance
# Data is held in chess-training-gobet96.dat and chess-test-gobet96.dat

# Written by Peter Lane, 2010.

require "java"
require "chrest"

import "jchrest.architecture.Chrest"
import "jchrest.gui.ChrestView"
import "jchrest.lib.ChessDomain"
import "jchrest.lib.Scenes"

# ---------------------------------------------------------------------
# Supporting methods

# Read in a set of Scene descriptions
def read_data filename
  scenes = []
  begin 
    fbr = java.io.BufferedReader.new(java.io.FileReader.new(filename))
    fbr.read_line # reads and ignores the 'visual search' line
    scenes = Scenes.read(fbr)
  rescue java.io.IOException 
    puts "Error !"
  end
  scenes
end

# Display scene
def display_scene scene
  scene.height.times do |h|
    scene.width.times do |w|
      print scene.get_item(h, w)
    end
    puts
  end
end

# Count the number of non empty squares in a scene
def count_items scene
  count = 0
  scene.height.times do |h|
    scene.width.times do |w|
      count += 1 unless scene.is_empty(h, w)
    end
  end
  count
end

# Train model on given scene
def learn_scene(model, scene, fixations=20)
  model.perceiver.scene = scene
  model.perceiver.start
  fixations.times do 
    model.perceiver.move_eye_and_learn
  end
end

# Scan given scene
def scan_scene(model, scene, fixations=20)
  model.perceiver.scene = scene
  model.perceiver.start
  fixations.times do
    model.perceiver.move_eye
  end
end

# Create a model, training on each scene n times
def create_model(scenes, cycles, fixations = 20, cap = 1_000_000)
  model = Chrest.new
  model.set_domain(ChessDomain.new)
  puts "Learning: "
  puts "Cycle  Visual LTM size  Avg depth  Avg image size"
  cycles.times do |cycle|
    scenes.get_scene_names.length.times do |i|
      break if model.ltm_size > cap 
      learn_scene(model, scenes.get(i), fixations)
    end
    puts "#{cycle+1} #{model.ltm_visual_size} #{"%.2f" % model.get_visual_ltm_average_depth} #{"%.2f" % model.get_visual_ltm_average_image_size}"
  end
  model
end

# Collect unique items from visual stm
def get_unique_stm_items model
  items = []
  model.visual_stm.count.times do |i|
    image = model.visual_stm.item(i).image
    image.size.times do |j|
      item = image.get_item j
      items << item if items.detect {|obj| obj.equalPrimitive(item)}.nil?
    end
  end
  items
end

# Count the number of recalled items which are correctly placed on the scene
def count_overlap(recalled_items, scene)
  count = 0
  recalled_items.each do |item|
    begin
      count += 1 if item.item == scene.get_item(item.row-1, item.column-1)
    rescue
    end
  end
  count
end

def compute_recall(recalled_items, scene)
  count_overlap(recalled_items, scene).quo(count_items(scene))
end

def compute_precision(recalled_items, scene)
  if (recalled_items.length.zero?)
    0
  else
    count_overlap(recalled_items, scene).quo(recalled_items.length)
  end
end

# Retrieve the number of unique fixated squares
def count_fixated_squares model
  squares = {}
  model.perceiver.get_number_fixations.times do |index|
    square_x = model.perceiver.get_fixations_x(index)
    square_y = model.perceiver.get_fixations_y(index)
    squares[[square_x,square_y]] = true
  end
  squares.size
end

# Retrieve the number of unique fixated squares with pieces
def count_fixated_pieces(model, scene)
  pieces = {}
  model.perceiver.get_number_fixations.times do |index|
    square_x = model.perceiver.get_fixations_x(index)
    square_y = model.perceiver.get_fixations_y(index)
    item = scene.get_item(square_x, square_y)
    pieces[[item, square_x, square_y]] = true
  end
  pieces.size
end

RecallResult = Struct.new(:target_items, 
                          :recalled_items, 
                          :num_correct, 
                          :recall, 
                          :precision,
                          :fixated_squares,
                          :fixated_pieces
                         )

# Compute recall performance of model and return data within RecallResult structure
def recall_performance(model, scene, fixations = 20)
  scan_scene(model, scene, fixations)
  recall = get_unique_stm_items model
  if Trace
    display_scene scene
    puts "Recalled: #{recall.join(",")}"
    puts "Correct is: #{count_overlap(recall, scene)}, Recall: #{compute_recall(recall, scene)}"
    puts "---------------------------------------------------"
  end
  RecallResult.new(count_items(scene), recall.length, count_overlap(recall, scene),
                  compute_recall(recall, scene), compute_precision(recall, scene),
                  count_fixated_squares(model), count_fixated_pieces(model, scene))
end

def average items
  items.inject(0, :+).to_f / items.length
end

def avg_recall results
  100*average(results.collect {|res| res.recall})
end

def avg_fbc results
  100*average(results.collect {|res| res.fixated_squares.quo(64)})
end

def avg_fpc results
  100*average(results.collect {|res| res.fixated_pieces.quo(res.target_items)})
end

# --------------------------------------------------------------------
# Reproduce the de Groot and Gobet (1996) experiments with CHREST

Trace = false # flag to display more information
TrainingData = read_data "../sample-data/chess-positions.dat"
TestData = read_data "chess-test-gobet96.dat"

@@novice_model = create_model(TrainingData, 2, 20, 200)
@@expert_model = create_model(TrainingData, 2) #, 20, 1000)

@@novice_results = []
TestData.get_scene_names.length.times do |i|
  @@novice_results << recall_performance(@@novice_model, TestData.get(i))
end

@@expert_results = []
TestData.get_scene_names.length.times do |i|
  @@expert_results << recall_performance(@@expert_model, TestData.get(i))
end

puts
puts "Overview: "
puts 
puts "Number of training positions: #{TrainingData.get_scene_names.length}"
puts "Number of test positions: #{TestData.get_scene_names.length}"
puts "Novice network size: #{@@novice_model.ltm_size} (#{@@novice_model.get_ltm.get_children.size} primitives)"
puts "Expert network size: #{@@expert_model.ltm_size} (#{@@expert_model.get_ltm.get_children.size} primitives)"

puts
puts "Experiment 1: Duration of Fixations"
puts 
puts "     Human Data            Simulations"
puts "     Masters Novices       CHREST-Master CHREST-Novice"
puts "mean 260     310 "
puts "sd   100     140 "
puts " (times in milli-seconds)"

puts
puts "Experiment 2: Percentage of Board Covered"
puts 
puts "     Human Data            Simulations"
puts "     Masters Novices       CHREST-Master CHREST-Novice"
puts "BC   91      69 "
puts "FBC  56      40            #{"%2.1d" % avg_fbc(@@expert_results)}            #{"%2.1d" % avg_fbc(@@novice_results)}"
puts "PC   92      69 "
puts "FPC  68      46            #{"%2.1d" % avg_fpc(@@expert_results)}            #{"%2.1d" % avg_fpc(@@novice_results)}"
puts 
puts "BC - board coverage  FBC - focussed board coverage "
puts "PC - piece coverage  FPC - focussed piece coverage "

puts
puts "Experiment 3: Relevance of Squares Covered"
puts
puts "     Human Data            Simulations "
puts "     Masters Novices       CHREST-Master CHREST-Novice"
puts "ESRC 0.36    0.07 "
puts "OSRC 0.33    0.03 "
puts "SRC  0.36    0.08 "
puts 
puts "ESRC - empty squares relevance coverage"
puts "OSRC - occupied squares relevance coverage"
puts "SRC  - square relevance coverage"

puts
puts "Experiment 4: Recall Performance"
puts 
puts "Human Data             Simulations"
puts "Masters Novices        CHREST-Master CHREST-Novice"
puts "82.9    20.5           #{"%2.1f" % avg_recall(@@expert_results)}          #{"%2.1f" % avg_recall(@@novice_results)}"

