// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

class Main
{
  static Void main (Str[] args)
  {
    if (args.size == 1)
    {
      runSingleModel (Int.fromStr(args[0]))
    }
    else
    {
      runExperiment
    }
  }
  
  static Void runSingleModel (Int num)
  {
    echo ("SEARCH")
    search := Search { trace = true; numChunks = num }
    search.start_episode
  }

  static Void runExperiment ()
  {
    echo ("SEARCH Experiment")
    for (Int i := 5000; i <= 150000; i += 5000)
    {
      result := runExperimentForSize (i)
      echo ("For size $i:")
      result.display
      echo ("")
    }
  }

  static Result runExperimentForSize (Int size)
  {
    result := Result()
    1000.times
    {
      search := Search { numChunks = size }
      search.start_episode
      result.addSearchDepth (search.movesMade)
      result.addTime (search.clock)
      result.addFuzziness (search.fuzziness)
    }
    return result
  }
}

class Result
{
  Int[] searchDepth := [,]
  Int[] times := [,]
  Int[] fuzzinesses := [,]

  Void addSearchDepth (Int depth)
  {
    searchDepth.add (depth)
  }

  Void addTime (Duration time)
  {
    times.add (time.toSec)
  }
  
  Void addFuzziness (Int fuzziness)
  {
    fuzzinesses.add (fuzziness)
  }
  
  Float avg (Int[] items)
  {
    if (items.isEmpty)
    {
      return 0.0f
    }
    else
    {
      total := 0.0f
      items.each { total += it }
      return total / items.size
    }
  }

  Void display ()
  {
    echo ("Avg depth: ${avg(searchDepth)}")
    echo ("Avg time: ${avg(times)}")
    echo ("Avg fuzziness: ${avg(fuzzinesses)}")
  }
}
