# Rake file for managing the Chrest project

VERSION = '4.0.0-alpha-1' # current version for bundling
JCOMMON = 'lib/jcommon-1.0.17.jar' # name of library
JFREECHART = 'lib/jfreechart-1.0.14.jar' # name of library
JRUBY = 'jruby-1.6.7.2' # name of jruby executable

directory 'bin'

desc 'compile Chrest classes into bin folder'
task :compile => 'bin' do
  Dir.chdir ('src/jchrest-architecture') do
    sh "javac -cp ../../#{JCOMMON}:../../#{JFREECHART} -d ../../bin `find -name \"*.java\"`"
  end
end

desc 'run Chrest, from compiled code'
task :run => :compile do
  sh "java -cp bin:#{JCOMMON}:#{JFREECHART} jchrest/gui/Shell"
end

directory 'tmp'

desc 'extract java libs into tmp directory'
task :extract_libs => 'tmp' do
  Dir.chdir('tmp') do
    sh "jar -xf ../#{JCOMMON}"
    sh "jar -xf ../#{JFREECHART}"
  end
end

desc 'build Chrest into a self-contained jar file'
task :make_jar => [:compile, :extract_libs] do 
  sh 'jar -cfm chrest.jar lib/Manifest lib/orange-chrest-logo.png -C bin . -C tmp .'
end

desc 'remove the bin/release/tmp directories'
task :clean do
  sh 'rm -rf bin'
  sh 'rm -rf tmp'
  sh 'rm -rf release'
end

desc 'build the user guide'
task :guide do
  Dir.chdir('doc/user-guide') do
    if File.stat('user-guide.tex').mtime > File.stat('user-guide.pdf').mtime
      sh 'latex user-guide'
      sh 'latex user-guide'
      sh 'latex user-guide'
      sh 'dvipdf user-guide.dvi'
    end
  end
end

desc 'show the user guide'
task :show_guide => :guide do
  Dir.chdir('doc/user-guide') do
    sh 'evince user-guide.pdf &'
  end
end

desc 'build the manual'
task :manual do
  Dir.chdir('doc/manual') do
    if File.stat('manual.tex').mtime > File.stat('manual.pdf').mtime
      sh 'latex manual'
      sh 'bibtex manual'
      sh 'latex manual'
      sh 'latex manual'
      sh 'dvipdf manual.dvi'
    end
  end
end

desc 'show the manual'
task :show_manual => :manual do
  Dir.chdir('doc/manual') do
    sh 'evince manual.pdf &'
  end
end

directory 'doc/api'
desc 'create API documentation'
task :api_doc => 'doc/api' do
  Dir.chdir('src/jchrest-architecture') do
    sh "javadoc -classpath ../../#{JCOMMON}:../../#{JFREECHART} -d ../../doc/api `find -name \"*.java\"`"
  end
end

directory 'release/chrest'
desc 'bundle for release'
task :bundle => [:guide, :manual, :make_jar, :api_doc, 'release/chrest'] do
  Dir.chdir('release/chrest') do
    sh 'rm -rf documentation' # remove it if exists already
    sh 'mkdir documentation'
    sh 'cp ../../lib/license.txt documentation'
    sh 'cp ../../doc/user-guide/user-guide.pdf documentation'
    sh 'cp ../../doc/manual/manual.pdf documentation'

    sh 'cp ../../chrest.jar .'
    sh 'cp -r ../../examples .'
    sh 'rm examples/lisp/abcl.jar'
    sh 'rm -rf examples/lisp/toreview'
    sh 'rm -rf examples/ruby/toreview'

    sh 'cp -r ../../doc/api documentation/javadoc'
    File.open("start-chrest.sh", "w") do |file|
      file.puts <<END
java -Xmx100M -jar chrest.jar
END
    end
    File.open("start-chrest.bat", "w") do |file|
      file.puts <<END
start javaw -Xmx100M -jar chrest.jar
END
    end
  end
  Dir.chdir('release') do
    sh "zip -FS -r chrest-#{VERSION}.zip chrest"
  end
end

desc 'run all Chrest tests'
task :test => :compile do
  Dir.chdir('src/tests') do
    sh "#{JRUBY} --1.9 -J-cp ../../bin all-chrest-tests.rb"
  end
end
