#!/bin/sh

# workaround for https://github.com/softprops/bintray-sbt/issues/25

mkdir -p ${HOME}/.bintray
echo "realm = Bintray API Realm" > ${HOME}/.bintray/.credentials
echo "host = api.bintray.com"  >> ${HOME}/.bintray/.credentials
echo "user = fommil" >> ${HOME}/.bintray/.credentials
echo "password = ${BINTRAY_TOKEN}"  >> ${HOME}/.bintray/.credentials
