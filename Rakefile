#! ruby
require 'fileutils'

SCALA_ROOT = "/home/aemon/lib/scala"
SCALA = "#{SCALA_ROOT}/bin/scala"
SCALAC = "#{SCALA_ROOT}/bin/scalac"

SOURCES = ["src/com/ensime/server/*.scala"]

CLASS_PATH = ["classes"]

task :clean => [] do
  FileUtils.rm_f Dir.glob('classes/*.class')
  FileUtils.rm_rf 'classes/com'
end

task :compile => [:clean] do
  system "#{SCALAC} -deprecation -sourcepath src -classpath #{CLASS_PATH} -d classes #{(SOURCES).join(' ')}"
end

task :compile_tests => [:clean] do
  system "#{SCALAC} -deprecation -classpath #{TEST_CLASS_PATH} -sourcepath src -d classes #{(SOURCES + TEST_SOURCES).join(' ')}"
end

task :test => [:compile_tests] do
  system "#{SCALA} -classpath #{TEST_CLASS_PATH} com.scala_anim.test.allSpecsRunner"
end

#task :repl => [:compile] do
#  PRELOAD = "import com.scala_anim._\nimport com.scala_anim.canvas._;"
#  File.open(".repl_preload", "w+"){|f|
#    f.write PRELOAD
#  }
#  system "scala -classpath #{CLASS_PATH} -i .repl_preload"
#  FileUtils.rm_rf '.repl_preload'
#end

task :run => [] do
  system "java -classpath #{SCALA_ROOT}/lib/scala-library.jar:#{CLASS_PATH} com.ensime.server.SExp \"(1 (hello out 1 there) one :dude hello)\""
end

task :run_server => [] do
  system "java -classpath #{SCALA_ROOT}/lib/scala-library.jar:#{SCALA_ROOT}/lib/scala-compiler.jar:#{CLASS_PATH} com.ensime.server.Server"
end

task :profile => [] do
  system "java -javaagent:c:/java_profile/profile/profile.jar -classpath classes/scala-library.jar;#{CLASS_PATH} com.scala_anim.MyAppRunner"
end

task :compile_and_run => [:compile, :run]

task :default => [:compile]
