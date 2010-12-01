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

if [ "$ENSIME_JVM_ARGS" == "" ]
then
  ENSIME_JVM_ARGS="-Xms256M -Xmx1024M"
fi

CLASSPATH=<RUNTIME_CLASSPATH>
CMD="java -classpath ${CLASSPATH} ${ENSIME_JVM_ARGS} org.ensime.server.Server ${PORT_FILE}"
echo $CMD
$CMD

