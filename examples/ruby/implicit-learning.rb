# Implicit learning task, using Reber grammar
#
# Model written by Peter Lane, July 2012.
#
# This script simulates the experiment described in Kennedy and Patterson (2012)
# and demonstrates how CHREST captures patterns of behaviour 
# typical of implicit learning.

require "chrest"

class Chrest
  def retrieve_chunks pat
    chunks = []
    node = recognise pat
    chunks << node.contents
    new_pat = pat.remove(node.contents)
    while not(new_pat.equals(pat))
      pat = new_pat
      node = recognise pat
      chunks << node.contents
      new_pat = pat.remove(node.contents)
    end
    return chunks
  end
end

Trace = false

# ValidStrings generated from Reber grammar 
# (figure 3 of Kennedy and Patterson (2012) - ICCM-12)
# Note: 43 valid strings obtained, not 44 as given in their paper

ValidStrings = <<END
TTS
VXS
TPTS
VXXS
VXPS
TPPTS
TTVXS
VXXXS
VXXPS
TPPPTS
TPTVXS
TTVXXS
TTVXPS
VXXXXS
VXXXPS
VXPVXS
TPPPPTS
TPPTVXS
TPTVXXS
TPTVXPS
TTVXXXS
TTVXXPS
VXXXXXS
VXXXXPS
VXXPVXS
VXPVXXS
VXPVXPS
TPPPPPTS
TPPPTVXS
TPPTVXXS
TPPTVXPS
TPTVXXXS
TPTVXXPS
TTVXXXXS
TTVXXXPS
TTVXPVXS
VXXXXXXS
VXXXXXPS
VXXXPVXS
VXXPVXXS
VXXPVXPS
VXPVXXXS
VXPVXXPS
END

def create_data
  patterns = ValidStrings.split("\n").collect do |str| 
    ChrestPattern.make_verbal_pattern_from_string(str)
  end
  patterns.shuffle!
  return [patterns[0...18], patterns[21..-1]]
end

def random_string n
  letters = ["T", "X", "P", "V"]
  word = ""
  while word.size < n-1
    word << letters.sample
  end
  result =  word + "S" # end with 'S'
  if ValidStrings.split("\n").include?(result)
    return random_string(n)
  else
    return result
  end
end

# create 22 random samples from characters in string
def rndTests
  result = []
  7.times do
    [6,7,8].each do |size|
      result << ChrestPattern.make_verbal_pattern_from_string(random_string(size))
    end
  end
  result << ChrestPattern.make_verbal_pattern_from_string(random_string(7))
  return result
end

# Train model

def train_model patterns
  model = Chrest.new

  1.times do
    patterns.each do |pat|
      model.recognise_and_learn pat
      node = model.recognise pat
      new_pat = pat.remove(node.contents)
      while not(new_pat.equals(pat))
        pat = new_pat
        model.recognise_and_learn pat
        node = model.recognise pat
        new_pat = pat.remove(node.contents)
      end
    end
  end
  return model
end

# Run each test twice
def test_model(model, patterns, valid)
  true_res = 0
  false_res = 0
  2.times do
    patterns.each do |pat|
      chunks = model.retrieve_chunks pat
      if Trace
        puts "For pattern #{pat.toString}"
        chunks.each do |chunk|
          puts " - " + chunk.toString
        end
      end
      # conditions to reject string
      # 1. number of small chunks is too large    (lack of knowledge)
      # 2. number of chunks in total is too large (articulatory loop)
      if chunks.inject(0) {|r, chunk| r + (chunk.size == 1 ? 1 : 0)} > 2 or
        chunks.size > 5
        if valid
          false_res += 1
        else
          true_res += 1
        end
      else
        if valid
          true_res += 1
        else
          false_res += 1
        end
      end
    end
  end
  return [true_res, false_res]
end

# Test model and report results

def run_expt
  training, testing = create_data
  model = train_model(training)
  true_pos, false_neg = test_model(model, testing, true)
  true_neg, false_pos = test_model(model, rndTests, false)
  return [true_pos, false_neg, true_neg, false_pos]
end

NumRuns = 100

def format a, b
  ("%5.2f" % (a.to_f / NumRuns)) + "/" + ("%5.2f" % ((a+b).to_f / NumRuns))
end

total_true_pos = 0
total_false_neg = 0
total_true_neg = 0
total_false_pos = 0

NumRuns.times do
  true_pos, false_neg, true_neg, false_pos = run_expt
  total_true_pos += true_pos
  total_false_neg += false_neg
  total_true_neg += true_neg
  total_false_pos += false_pos
end

puts
puts "*** Results ***"
puts
puts "Valid Invalid (Actual)"
puts "#{"%5d" % total_true_pos} #{"%5d" % total_false_neg}    | Valid    (True)"
puts "#{"%5d" % total_false_pos} #{"%5d" % total_true_neg}    | Invalid"
puts 
puts "                    Human  ACT-R  CHREST"
puts "-----------------------------------------------------"
puts "              Hits: 33/44  34/44  #{format(total_true_pos, total_false_neg)}"
puts "Correct rejections: 36/44  39/44  #{format(total_true_neg, total_false_pos)}"
puts "            Misses: 11/44  10/44  #{format(total_false_neg, total_true_pos)}"
puts "      False alarms:  8/44   5/44  #{format(total_false_pos, total_true_neg)}"
puts
puts "(Human/ACT-R from Kennedy & Patterson (2012).  "
puts " CHREST results averaged over 100 runs.)"

