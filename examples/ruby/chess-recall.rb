# Simplest illustration of using CHREST in recall experiment
#
# 1. Load some chess data, and separate into train/test sets
# 2. Create different sized models, from small to large
# 3. Demonstrate improved average recall performance over size
# 4. Demonstrate increase in recall performance when using templates

# Script written by Peter Lane, 2012

require "chrest"

# Target model sizes
Sizes = [200, 10_000, 50_000, 100_000] #, 100_000, 300_000]

# 1. Data set
DataSet = Scenes.read_from_file "../sample-data/chess-positions.dat"
TrainSet = []
TestSet = []
# Randomly separate out train/test sets
DataSet.size.times do |i|
  if rand < 0.9
    TrainSet << DataSet.get(i)
  else
    TestSet << DataSet.get(i)
  end
end

# add required method to trainSet for use in 'create_model'
def TrainSet.get(i)
  self[i]
end

# 2. Models

Models = {}
Sizes.each do |size| 
  Models[size] = Chrest.create_model(TrainSet, 10, 20, size)
end

# 3. Recall performance

Results = Struct.new(:recall, :precision, :omission, :commission)

def format_result n
  "%6.2f" % (if n.zero? then 0 else n.to_f/TestSet.size end)
end

def recall_result size, performances
  format_result performances[size].recall
end

def precision_result size, performances
  format_result performances[size].precision
end

def omission_result size, performances
  format_result performances[size].omission
end

def commission_result size, performances
  format_result performances[size].commission
end

def evaluate model, size, performances
  TestSet.each do |scene|
    recalled = model.scan_scene scene, 20
    performances[size].recall += scene.compute_recall recalled
    performances[size].precision += scene.compute_precision recalled
    performances[size].omission += scene.compute_errors_of_omission recalled
    performances[size].commission += scene.compute_errors_of_commission recalled
  end
end

def show_result type, performances
  print "%20s:" % type.capitalize
  Sizes.each do |size|
    print " "
    print send("#{type}_result", size, performances)
  end
  puts 
end

def evaluate_models str=""
  performances = {}
  Sizes.each do |size| 
    performances[size] = Results.new(0, 0, 0, 0)
    evaluate Models[size], size, performances
  end

  puts
  puts "Average recall performance #{str}"
  puts
  print "                      "
  Sizes.each do |size|
    print "#{"%6d" % size} "
  end
  puts
  print "                      "
  Sizes.each do |size|
    print "------ "
  end
  puts

  Results.members.map {|m| show_result(m, performances)}
end

evaluate_models "- no templates"

# 4. Construct templates for all models, and re-evaluate

Sizes.each do |size|
  Models[size].construct_templates
end
evaluate_models "- with templates"
# -- display number of templates per model
print " Number of templates: "
Sizes.each do |size|
  print "#{"%6d" % Models[size].count_templates} "
end
puts

