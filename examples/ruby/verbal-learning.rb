require "chrest"

# This file demonstrates some results from the verbal-learning literature. 
# These results were key to the early success of EPAM as a model of human 
# learning.  They still provide convincing evidence that human memory is 
# indexed using a discrimination-net type of index.
#
# For comparison, results for humans, EPAM III and EPAM VI are shown, 
# taken from H. Richman, H.A. Simon, E.A. Feigenbaum, 'Simulations of 
# paired associate learning using EPAM VI', Working paper 553, 2002.
#
# 1. Effect of stimulus and response familiarisation
# 2. Constant learning time
# 3. Effect of inter-list simularity

puts "Verbal Learning Experiments"
puts

# Experiment 1 : Effect of stimulus and response familiarisation
#
# Aim:
# Simulate result of Chenzoff (1962) that pre-familiarisation of responses 
# is more important than pre-familiarisation of stimuli.

# Stimulus-Response pairs
# (taken from EPAM-VI file, Underwood's low-low condition)
Pairs = ChrestPattern.makeSRPairs [
  ["xin", "vod"],
  ["toq", "hax"], 
  ["wep", "cem"], 
  ["duf", "jyl"], 
  ["myd", "siq"], 
  ["ruk", "fec"], 
  ["nas", "baj"], 
  ["pov", "loz"], 
  ["kir", "zub"],
  ["gac", "yug"]
]

# train model on given patterns until it recalls them all
# -- timeout is the number of attempts made until it gives up
def train_model(model, patterns, timeout = 100)
  cycle = 0
  begin
    some_unknown = false
    cycle += 1
    patterns.each do |pattern|
      if model.recall_pattern(pattern).nil? or 
        !(model.recall_pattern(pattern).java_send(:equals, [Java::jchrest.lib.ListPattern], pattern))
        some_unknown = true
        model.recognise_and_learn pattern
      end
    end
  end while some_unknown and cycle <= timeout
  return cycle # return the number of training cycles required
end

def show_responses(model, pairs)
  pairs.each do |stimulus, response|
    print "For #{stimulus.toString}-#{response.toString} model returns "
    print "(#{model.recognise(stimulus).contents})#{model.recognise(stimulus).image.toString}-"
    if model.assocated_pattern(stimulus).nil?
      puts "nothing"
    else
      puts "#{model.associated_pattern(stimulus).toString}"
    end
  end
end

# train model on given SR pairs until it gets them right
# -- timeout is the number of attempts made until it gives up
# Returns the number of cycles required
def train_model_pairs(model, pairs, timeout = 100)
  errors = 0
  cycle = 0
  begin
    some_unknown = false
    cycle += 1
    pairs.each do |stimulus, response|
      if model.associate_pattern(stimulus).nil? or 
        !(model.associate_pattern(stimulus).java_send(:equals, [Java::jchrest.lib.ListPattern], response))
        some_unknown = true
        errors += 1
        model.associate_and_learn(stimulus, response)
      end
    end
  end while some_unknown and cycle <= timeout
  return cycle
end

def create_model
  model = Chrest.new
  return model
end

def train_u_u_condition
  model = create_model
  return train_model_pairs(model, Pairs)
end

def train_f_u_condition
  model = create_model
  train_model(model, Pairs.collect{|p| p[0]})
  return train_model_pairs(model, Pairs)
end

def train_u_f_condition
  model = create_model
  train_model(model, Pairs.collect{|p| p[1]})
  return train_model_pairs(model, Pairs)
end

def train_f_f_condition
  model = create_model
  train_model(model, Pairs.flatten)
  return train_model_pairs(model, Pairs)
end

uu = train_u_u_condition
fu = train_f_u_condition
uf = train_u_f_condition
ff = train_f_f_condition

puts "Experiment 1:"
puts "Effects of Stimulus and Response Familiarisation"
puts
puts "Table of Trials to learn list"
puts
puts "Condition  People  EPAM III  EPAM VI  CHREST"
puts "---------  ------  --------  -------  ------"
puts "   F-F      1.0     1.0       1.0      #{ff.to_f/ff}"
puts "   U-F      1.2     1.3       1.9      #{uf.to_f/ff}"
puts "   F-U      1.6     1.8       2.8      #{fu.to_f/ff}"
puts "   U-U      1.8     2.5       3.7      #{uu.to_f/ff}"
puts

# Experiment 2 : Constant Learning Time
#
# Aim:
# Simulate result of Bugelski (1962) that total time to learn a 
# paired-associate list is not affected by presentation rate.

# Class represents and handles an Experiment
class Experiment
  attr_reader :cycles

  def initialize(items, presentation_time, initial_clock = 0)
    @clock = initial_clock
    @current = 0
    @cycles = 0
    @items = items
    @presentation_time = presentation_time
  end

  def num_items
    @items.length
  end

  def [](index)
    @items[index]
  end

  def current_item
    @items[@current]
  end

  def starting_new_cycle?
    @current.zero?
  end

  def next_item
    @current += 1
    @clock += @presentation_time
    if @current >= @items.length - 1
      @current = 0
      @cycles += 1
    end
  end

  def perform_cycle model
    loop do
      node = model.recognise(@items[@current])
      if node.associated_node.nil?
        model.associate_and_learn(@items[@current], @items[@current+1], @clock)
      else
        if node.associated_node.image.java_send(:equals, [Java::jchrest.lib.ListPattern], @items[@current+1])
          if rand < 0.3 # try overlearning
            loops = 0
            while model.clock < @clock and loops < 10
              if rand < 0.5
                model.recognise_and_learn(@items[@current])
              else
                model.recognise_and_learn(@items[@current+1])
              end
              loops += 1
            end
          end
        else
          model.associate_and_learn(@items[@current], @items[@current+1], @clock)
        end
      end
     next_item
      break if starting_new_cycle?
    end
  end
end

# -- start of experiment

def not_fully_learnt?(model, experiment) 
  0.upto(experiment.num_items-2) do |index|
    retrieved_pattern = model.associate_pattern(experiment[index])
    return true if retrieved_pattern.nil? 
    return true unless retrieved_pattern.java_send(:equals, [Java::jchrest.lib.ListPattern], (experiment[index+1]))
  end
  return false # everything is learnt completely
end

def train_to_success(model, experiment, safety_net = 1000)
  while not_fully_learnt?(model, experiment) do
    experiment.perform_cycle model
    break if safety_net.zero?
    safety_net -= 1
  end
end

# List of stimuli (taken from EPAM-VI file)
BugelskiList = [
  "gey", "nur", "kar", "weh", "bih", "xir", "cez", "mun", "fax",
  "soq", "tof", "lah", "dup", "tez", "gac", "qet"
].collect {|p| ChrestPattern.make_pattern_from_string(p)}

ResultsTrials = {}
ResultsTimes = {}
NumTrials = 10

[6000, 8000, 10000, 12000, 19000].each do |time|
  ResultsTrials[time] = 0
  ResultsTimes[time] = 0
  NumTrials.times do
    expt = Experiment.new(BugelskiList, time)
    model = Chrest.new
    model.discrimination_time = 8000
    model.familiarisation_time = 4000
    model.add_link_time = 8000
    model.rho = 0.7
    train_to_success(model, expt)
    ResultsTrials[time] += expt.cycles
    ResultsTimes[time] += model.clock/(1000.0*BugelskiList.length)
  end
  ResultsTrials[time] = ResultsTrials[time]/NumTrials.to_f
  ResultsTimes[time] = ResultsTimes[time]/NumTrials.to_f
end

puts "Experiment 2:  *** IN PROGRESS *** "
puts "Constant Learning Time"
puts
puts "Table of Mean trials and total times to learn lists"
puts
puts "                   People                EPAM VI         \
     CHREST"
puts "Presentation  --------------------  -------------------- \
--------------------"
puts "    Time      Trials Exposure-Time  Trials Exposure-Time \
Trials Exposure-Time"
puts "------------  ------ -------------  ------ ------------- \
------ -------------"
puts "    6 sec      10.2        61.2       9.3        55.6     \
#{"%4.1f" % ResultsTrials[6000]}     #{"%4.1f" % ResultsTimes[6000]}"
puts "    8 sec       8.8        70.1       7.2        57.8     \
#{"%4.1f" % ResultsTrials[8000]}     #{"%4.1f" % ResultsTimes[8000]}"
puts "   10 sec       5.8        57.9       5.9        58.6     \
#{"%4.1f" % ResultsTrials[10000]}     #{"%4.1f" % ResultsTimes[10000]}"
puts "   12 sec       4.7        56.1       5.0        59.5     \
#{"%4.1f" % ResultsTrials[12000]}     #{"%4.1f" % ResultsTimes[12000]}"
puts "   19 sec       3.3        62.2       3.5        65.9     \
#{"%4.1f" % ResultsTrials[19000]}     #{"%4.1f" % ResultsTimes[19000]}"
puts


# Experiment 3 : Effect of Intra-List Similarity of Stimuli and Responses
#
# Aim: 
# Simulate result of Underwood (1953) that intralist similarity of stimuli 
# effects learning rate, but not intralist similarity of responses.

PairsLowLow = ChrestPattern.makeSRPairs [
  ["xin", "vod"],
  ["toq", "hax"], 
  ["wep", "cem"], 
  ["duf", "jyl"], 
  ["myd", "siq"], 
  ["ruk", "fec"], 
  ["nas", "baj"], 
  ["pov", "loz"], 
  ["kir", "zub"],
  ["gac", "yug"]
]
PairsLowMedium = ChrestPattern.makeSRPairs [
  ["xin", "hiz"], 
  ["toq", "vec"], 
  ["wep", "vir"], 
  ["duf", "juw"], 
  ["myd", "hul"], 
  ["ruk", "fec"], 
  ["nas", "yor"], 
  ["pov", "jal"], 
  ["kir", "foz"],
  ["gac", "yaw"]
]
PairsLowHigh = ChrestPattern.makeSRPairs [
  ["xin", "hux"], 
  ["toq", "hex"], 
  ["wep", "yal"], 
  ["duf", "yor"], 
  ["myd", "jir"], 
  ["ruk", "yol"], 
  ["nas", "jax"], 
  ["pov", "jix"], 
  ["kir", "jer"],
  ["gac", "hul"]
]
PairsMediumLow = ChrestPattern.makeSRPairs [
  ["hiz", "xin"], 
  ["vec", "toq"], 
  ["vir", "wep"], 
  ["juw", "duf"], 
  ["hul", "myd"],
  ["fec", "ruk"],
  ["yor", "nas"], 
  ["jal", "pov"], 
  ["foz", "kir"], 
  ["yaw", "gac"]
]
PairsHighLow = ChrestPattern.makeSRPairs [
  ["hux", "xin"],
  ["hex", "toq"], 
  ["yal", "wep"], 
  ["yor", "duf"], 
  ["jir", "myd"],
  ["yol", "ruk"], 
  ["jax", "nas"], 
  ["jix", "pov"], 
  ["jer", "kir"], 
  ["hul", "gac"]
]

def run_expt_3 pairs 
  total = 0
  10.times do
    total += run_expt_3_single pairs
  end
  return total.to_f/10
end

def run_expt_3_single(pairs, presentation_time = 19000, timeout = 100)
  model = Chrest.new
  model.discrimination_time = 8000
  model.familiarisation_time = 2000
  model.add_link_time = 8000
  model.rho = 0.7
  errors = 0
  cycle = 0
  clock = 0
  begin
    some_unknown = false
    cycle += 1
    clock += presentation_time
    pairs.each do |stimulus, response|
      if model.associate_pattern(stimulus).nil? or 
        !(model.associate_pattern(stimulus).java_send(:equals, [Java::jchrest.lib.ListPattern], response))
        some_unknown = true
        errors += 1
        model.associate_and_learn(stimulus, response, clock)
      end
    end
  end while some_unknown and cycle <= timeout
  return cycle
end

Expt3Results = {}
Expt3Results["low-low"] = run_expt_3 PairsLowLow
Expt3Results["low-medium"] = run_expt_3 PairsLowMedium
Expt3Results["low-high"] = run_expt_3 PairsLowHigh
Expt3Results["medium-low"] = run_expt_3 PairsMediumLow
Expt3Results["high-low"] = run_expt_3 PairsHighLow

puts "Experiment 3:"
puts "Effect of Intra-List Similarity of Stimuli and Responses"
puts
puts "Table of Mean trials to learn as a function of stimulus and response similarity"
puts
puts "                         EPAM III              EPAM VI         CHREST"
puts "                      ---------------  ----------------------        "
puts "Condition    People   Normal CV-Group    Quick      With-STM         " 
puts "---------- ---------- ------ --------  ----------- ----------  -------------"
puts "Low-Low    100 (23.2)   100     100    100 (13.0)  100 (13.4)   \
#{"%5.1f" % (Expt3Results['low-low']*100.0/Expt3Results['low-low'])} (#{Expt3Results['low-low']})"
puts "Low-Medium  96 (22.4)    88     100     98 (12.8)   97 (13.0)   \
#{"%5.1f" % (Expt3Results['low-medium']*100.0/Expt3Results['low-low'])} (#{Expt3Results['low-medium']})"
puts "Low-High   105 (24.4)    91     100     97 (12.6)   97 (13.0)   \
#{"%5.1f" % (Expt3Results['low-high']*100.0/Expt3Results['low-low'])} (#{Expt3Results['low-high']})" 
puts "Medium-Low 110 (25.5)   140     100    118 (15.3)  114 (15.3)   \
#{"%5.1f" % (Expt3Results['medium-low']*100.0/Expt3Results['low-low'])} (#{Expt3Results['medium-low']})"
puts "High-Low   132 (30.7)   146     114    124 (16.2)  120 (16.0)   \
#{"%5.1f" % (Expt3Results['high-low']*100.0/Expt3Results['low-low'])} (#{Expt3Results['high-low']})"
puts 

# ==========================================================================
# Simple graphical demonstration of networks
def show_trees
  model1 = Chrest.new
  train_model_pairs(model1, ChrestPattern.makeSRPairs([
                    ["xin", "vod"],
                    ["toq", "hax"], 
                    ["wep", "cem"], 
                    ["duf", "jyl"], 
                    ["myd", "siq"], 
                    ["ruk", "fec"], 
                    ["nas", "baj"], 
                    ["pov", "loz"], 
                    ["kir", "zub"],
                    ["gac", "yug"]
  ]))
  ChrestView.new(model1)

  model2 = Chrest.new
  train_model_pairs(model2, ChrestPattern.makeSRPairs([
                                                      ["hux", "xin"],
                                                      ["hex", "toq"], 
                                                      ["yal", "wep"], 
                                                      ["yor", "duf"], 
                                                      ["jir", "myd"],
                                                      ["yol", "ruk"], 
                                                      ["jax", "nas"], 
                                                      ["jix", "pov"], 
                                                      ["jer", "kir"], 
                                                      ["hul", "gac"]
  ]))
  ChrestView.new(model2)

end

# show_trees
