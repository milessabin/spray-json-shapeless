#!bash
WARNING="This file was generated using gen_manual.sh. Do not manually edit!"
PROTOCOL_DATA_DOCS=`python gen_protocol_docs.py data`
PROTOCOL_RPC_DOCS=`python gen_protocol_docs.py rpc`
cd ..
CONFIG_DOCS_FILE=/tmp/config_docs.out
sbt "run $CONFIG_DOCS_FILE"
cd etc
CONFIG_DOCS=`cat $CONFIG_DOCS_FILE`

m4 --define=NO_MANUAL_EDIT_WARNING="$WARNING" --define=PROTOCOL_DATA_DOCUMENTATION="$PROTOCOL_DATA_DOCS" --define=PROTOCOL_RPC_DOCUMENTATION="$PROTOCOL_RPC_DOCS" --define CONFIG_DOCUMENTATION="$CONFIG_DOCS" manual.ltx.m4 > manual.ltx
