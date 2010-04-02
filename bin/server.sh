#!/bin/bash
set -f
PORT_FILE=$1
CLASSPATH=dist/ensime.jar:lib/scala/scala-library.jar:lib/scala/scala-compiler.jar:lib/jnotify/jnotify-0.93.jar:lib/configgy/configgy-1.5.jar
java -classpath $CLASSPATH -Djava.library.path=lib/jnotify com.ensime.server.Server $PORT_FILE