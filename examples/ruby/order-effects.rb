# Order Effects experiment
# copyright (c) Peter C. R. Lane, 2010.
#
# See chapter 8 of 'In Order to Learn' (edited by F. Ritter et al), 2007.

require "chrest"

# Convenience function to convert a ruby array of integers into 
# Java ArrayList<int>, before passing it to Pattern to construct 
# a ListPattern
def make_number_pattern array
  Pattern.make_visual_list(array.to_java(:int))
end

# generate a random digit, in range 0..largest
def rnd_item(largest = 5)
  rand largest
end

# generate a list of random items, where list length is in range 1..length
def rnd_list(length = 10)
  (1+rand(length-1)).times.collect { rnd_item}
end

# generate a fixed-length list of patterns made from random lists
def generate_list length
  length.times.collect { make_number_pattern(rnd_list) }
end

# sort list of lists with shortest first
def sort_shortest list
  list.clone.sort {|a,b| a.size <=> b.size}
end

# sort list of lists with longest first
def sort_longest list
  list.clone.sort {|a,b| b.size <=> a.size}
end

# Four sets of results are kept for each experiment
Results = Struct.new(:ltm_size, :average_depth, :exact_matches, :partial_matches)

# Results are stored in separate HashMaps by list-length for 
# the random, smallest-first and largest-first conditions,

@@random_order = {}
@@shortest_first = {}
@@longest_first = {}

# Train a Chrest model on the given patterns, collect some 
# data on the network and performance, and save the results
def run_expt(patterns, results)
  model = Chrest.new
  # -- train the model on each pattern once in turn
  patterns.each do |pat|
    model.recognise_and_learn pat
  end
  # -- count the number of exact and partial matches
  exact_matches = 0
  partial_matches = 0
  patterns.each do |pat|
    retrieved = model.recall_pattern pat
    pat = pat.clone
    pat.set_not_finished
    retrieved = retrieved.clone
    retrieved.set_not_finished
    if (pat.matches(retrieved) and retrieved.matches(pat))
      exact_matches += 1
    elsif (pat.matches(retrieved))
      partial_matches += 1
    end
  end
  # -- store information on this experiment
  results[patterns.length] = Results.new(
    model.ltm_visual_size, 
    model.get_visual_ltm_average_depth,
    exact_matches,
    partial_matches
  )
end

def build_and_test_model patterns
  run_expt(patterns, @@random_order)
  run_expt(sort_shortest(patterns), @@shortest_first)
  run_expt(sort_longest(patterns), @@longest_first)
end

# Run the experiment
# -- two parameters
MAX_LEN   = 10000 # number of patterns to generate
PARTITION = 1000  # proportion of patterns to take each time
# -- data to use 
NumberList = generate_list MAX_LEN

PARTITION.step(MAX_LEN, PARTITION) do |size|
  build_and_test_model NumberList.slice(0, size)
end

# Output results
def print_results(name, results)
  puts name
  puts "List size, Ltm size, Ltm depth, Exact, Partial"
  PARTITION.step(MAX_LEN, PARTITION) do |size|
    puts "%9d,%9d,%10.2f,%6.2f,%8.2f" % [size,
      results[size].ltm_size,
      results[size].average_depth,
      results[size].exact_matches.quo(size),
      results[size].partial_matches.quo(size)
    ]
  end
  puts
end

print_results("Random order:", @@random_order)
print_results("Shortest first:", @@shortest_first)
print_results("Longest first:", @@longest_first)

