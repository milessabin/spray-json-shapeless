#!/bin/bash
set -f

case "$1" in
  -h|--help|"")
    echo "$0 should be run by emacs plugin. M-x ensime should start the server for you"
    exit 1
  ;;
  *)
    PORT_FILE=$1
  ;;
esac

INITIAL_HEAP=256M
MAX_HEAP=1024M
CLASSPATH=<RUNTIME_CLASSPATH>
java -classpath $CLASSPATH -Xms${INITIAL_HEAP} -Xmx${MAX_HEAP} org.ensime.server.Server $PORT_FILE
