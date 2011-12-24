#!bash

PROTOCOL_DOCS=`python gen_protocol_docs.py`
cd ..
CONFIG_DOCS_FILE=/tmp/config_docs.out
sbt "run $CONFIG_DOCS_FILE"
cd etc
CONFIG_DOCS=`cat $CONFIG_DOCS_FILE`

m4 --define=PROTOCOL_DOCUMENTATION=PROTOCOL_DOCS --define CONFIG_DOCUMENTATION=CONFIG_DOCS manual.ltx.m4 > manual.ltx
