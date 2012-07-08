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

# trace model training, training on each scene n times
def trace_model_development(scenes, cycles, report_frequency = 1000, fixations = 20, cap = 1_000_000)
  model = Chrest.new
  next_report = 0
  model.set_domain(ChessDomain.new)
  model.set_template_construction_parameters(5, 2)
  puts "Learning: (Occurrences: #{Chrest.MIN_OCCURRENCES} Level: #{Chrest.MIN_LEVEL})"
  cycles.times do |cycle|
    scenes.get_scene_names.length.times do |i|
      if model.ltm_size > next_report
        puts "#{model.ltm_size} #{"%.2f" % model.get_visual_ltm_average_depth} #{"%.2f" % model.get_visual_ltm_average_image_size} #{model.count_potential_templates}"
    #    model.show_potential_templates
        next_report += report_frequency
      end
      break if model.ltm_size > cap 
      learn_scene(model, scenes.get(i), fixations)
    end
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

# ---------------------------------------------------------
# Experiment with the training of chess models, looking at 
# properties of the network

Trace = false 
TrainingData = read_data "../sample-data/chess-positions.dat"

@@model = create_model(TrainingData, 5, 20, 1000000)

