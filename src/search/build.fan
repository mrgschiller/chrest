using build

class Build : BuildPod
{
  new make ()
  {
    podName = "searchSimulation"
    summary = "An implementation of SEARCH."
    depends = ["sys 1.0"]
    srcDirs = [`fan/`]
    version = Version([0, 1])
    docSrc = true
  }
}
