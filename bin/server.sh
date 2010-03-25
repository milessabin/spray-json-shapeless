#!/bin/bash
PORT_FILE=$1
PROJECT_ROOT_DIR=$2
PROJECT_SRC_DIR=$3
PROJECT_CLASSPATH=$4
CLASSPATH=classes:lib/scala/scala-library.jar:lib/scala/scala-compiler.jar:lib/jnotify/jnotify-0.93.jar:lib/configgy/configgy-1.5.jar
java -classpath $CLASSPATH -Djava.library.path=lib/jnotify com.ensime.server.Server $PORT_FILE $PROJECT_ROOT_DIR $PROJECT_SRC_DIR $PROJECT_CLASSPATH