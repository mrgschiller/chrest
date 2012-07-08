# Verbal Learning : Bugelski experiment

# This experiment reproduces result of Bugelski 1962 that the number of cycles
# through a list until it is completely learnt is inversely proportional to the
# presentation time.

require "java"
require "jchrest"

include_class "jchrest.architecture.Chrest"
include_class "jchrest.gui.ChrestView"
include_class "jchrest.lib.Pattern"

# -- utility

def make_patterns strings
  strings.collect { |str| Pattern.makeVisualList(str.split("").to_java(:String)) }
end

# Class represents and handles an Experiment
class Experiment
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
      model.learn_and_link_patterns(current_item, @items[@current+1], @clock)
      next_item
      break if starting_new_cycle?
    end
  end
end

# -- start of experiment

Patterns = make_patterns [ "DAG", "BIF", "GIH", "JAL", "MIQ", "PEL", "SUJ" ]

def not_fully_learnt?(model, experiment) 
  (1...experiment.num_items).each do |index|
    retrieved_pattern = model.follow_pattern(experiment[index])
    return true if retrieved_pattern.nil? or retrieved_pattern.equals(experiment[index+1])
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

expt = Experiment.new(Patterns, 500)
model = Chrest.new

train_to_success(model, expt)

ChrestView.new(nil, model)

