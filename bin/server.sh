#!/bin/bash
set -f
PORT_FILE=$1
CLASSPATH=lib/scala/scala-library.jar:lib/scala/scala-compiler.jar:dist/ensime.jar
java -classpath $CLASSPATH -Djava.library.path=lib/jnotify com.ensime.server.Server $PORT_FILE