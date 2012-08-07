using build

class Build : BuildScript
{
  @Target Void distFibonacci ()
  {
    dist := JarDist (this)
    dist.outFile = `./search.jar`.toFile.normalize
    dist.podNames = Str["searchSimulation"]
    dist.mainMethod = "searchSimulation::Main.main"
    dist.run
  }
}
