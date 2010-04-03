#! ruby
require 'fileutils'

SCALA_ROOT = "/home/aemon/lib/scala"
SCALA = "#{SCALA_ROOT}/bin/scala"
SCALAC = "#{SCALA_ROOT}/bin/scalac"
JAR = "jar"

SOURCES = FileList["src/**/*.scala"]

JVM_CLASSPATH = ["lib/jnotify/jnotify-0.93.jar",
                 "lib/configgy/configgy-1.5.jar",
                 "#{SCALA_ROOT}/lib/scala-library.jar",
                 "#{SCALA_ROOT}/lib/scala-compiler.jar",
                ].join(":")


task :clean => [] do
  FileUtils.rm_rf COMMON_TARGET
  FileUtils.rm_rf FileList["classes/*"]
  FileUtils.rm_rf FileList["dist/*"]
end

COMMON_TARGET = "classes/.common"
file COMMON_TARGET => SOURCES do
  sh "#{SCALAC} -deprecation -unchecked -sourcepath src -classpath #{JVM_CLASSPATH} -d classes #{(SOURCES).join(' ')}"
  touch COMMON_TARGET
end

task :compile => [:clean, COMMON_TARGET]

task :jar => [:compile] do
  Dir.chdir("classes"){
    sh "jar cf ../dist/ensime.jar *"
  }
end

task :default => [:jar]
