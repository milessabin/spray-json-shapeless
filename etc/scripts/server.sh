#!/bin/bash
set -f
PORT_FILE=$1
CLASSPATH=lib/scala-library.jar:lib/scala-compiler.jar:lib/ensime.jar:lib/ivy-2.1.0.jar
java -classpath $CLASSPATH com.ensime.server.Server $PORT_FILE