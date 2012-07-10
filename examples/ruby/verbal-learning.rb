require "chrest"

# This file demonstrates some classic results from the verbal-learning literature. 
# These results were key to the early success of EPAM.
#
# 1. Effect of stimulus and response familiarisation
# 2. Constant learning time
# 3. Effect of inter-list simularity

puts "Verbal Learning Experiments"
puts

# Experiment 1 : Effect of stimulus and response familiarisation
#
# Aim:
# Simulate result of Chenzoff (1962)

# Stimulus-Response pairs
# (taken from EPAM-VI, Underwood's low-low condition)
Pairs = [
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
].collect {|s, r| [ChrestPattern.make_pattern_from_string(s), ChrestPattern.make_pattern_from_string(r)]}

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

# train model on given SR pairs until it gets them right
# -- timeout is the number of attempts made until it gives up
# Returns the number of errors made
def train_model_pairs(model, pairs, timeout = 100)
  errors = 0
  cycle = 0
  begin
    some_unknown = false
    cycle += 1
    pairs.each do |stimulus, response|
      if model.follow_pattern(stimulus).nil? or 
        !(model.follow_pattern(stimulus).java_send(:equals, [Java::jchrest.lib.ListPattern], response))
        some_unknown = true
        errors += 1
        model.learn_and_link_patterns(stimulus, response)
      end
    end
  end while some_unknown and cycle <= timeout
  return errors
end

def create_model
  model = Chrest.new
  model.rho = 0.7
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
puts "Condition  People  CHREST"
puts "---------  ------  ------"
puts " F-F        1.0     #{ff.to_f/ff}"
puts " U-F        1.2     #{uf.to_f/ff}"
puts " F-U        1.6     #{fu.to_f/ff}"
puts " U-U        1.8     #{uu.to_f/ff}"
