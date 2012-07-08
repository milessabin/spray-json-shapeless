#!bash
WARNING="This file was generated using gen_manual.sh. Do not manually edit!"
MODIFIED_DATE=`date "+%m/%d/%Y"`
PROTOCOL_SOURCE="../src/main/scala/org/ensime/protocol/SwankProtocol.scala"
CONFIG_SOURCE="../src/main/scala/org/ensime/config/ProjectConfig.scala"
PROTOCOL_DATA_DOCS=`python gen_protocol_docs.py data $PROTOCOL_SOURCE`
PROTOCOL_RPC_DOCS=`python gen_protocol_docs.py rpc $PROTOCOL_SOURCE`
PROTOCOL_EVENTS_DOCS=`python gen_protocol_docs.py events $PROTOCOL_SOURCE`
PROTOCOL_VERSION=`python gen_protocol_docs.py version $PROTOCOL_SOURCE`
PROTOCOL_CHANGE_LOG=`python gen_protocol_docs.py changelog $PROTOCOL_SOURCE`
CONFIG_DOCS=`python gen_protocol_docs.py property $CONFIG_SOURCE`

m4 --define=NO_MANUAL_EDIT_WARNING="$WARNING" \
    --define=PROTOCOL_DATA_DOCUMENTATION="$PROTOCOL_DATA_DOCS" \
    --define=PROTOCOL_RPC_DOCUMENTATION="$PROTOCOL_RPC_DOCS" \
    --define=PROTOCOL_EVENTS_DOCUMENTATION="$PROTOCOL_EVENTS_DOCS" \
    --define=PROTOCOL_CHANGE_LOG="$PROTOCOL_CHANGE_LOG" \
    --define=CONFIG_DOCUMENTATION="$CONFIG_DOCS" \
    --define=PROTOCOL_VERSION="$PROTOCOL_VERSION" \
    --define=MODIFIED_DATE="$MODIFIED_DATE" \
    manual.ltx.m4 > manual.ltx

echo "Wrote updated manual.ltx"

echo "Converting manual to html.."
TMP_TARGET="/tmp/ensime_manual.html"
pdflatex manual.ltx
cat manual_head.html > $TMP_TARGET
tth -r -u -e2 -Lmanual < manual.ltx >> $TMP_TARGET
cat manual_tail.html >> $TMP_TARGET

ORIGIN="https://github.com/aemoncannon/ensime.git"
PAGES_BRANCH="gh-pages"

rm -rf $PAGES_BRANCH
mkdir $PAGES_BRANCH

echo "Getting latest $PAGES_BRANCH"
cd $PAGES_BRANCH
git init
git remote add -t $PAGES_BRANCH -f origin $ORIGIN
git checkout $PAGES_BRANCH
cd ..

echo "Copying content to $PAGES_BRANCH"
cp $TMP_TARGET $PAGES_BRANCH/index.html
cp wire_protocol.png $PAGES_BRANCH
cp manual.pdf $PAGES_BRANCH

echo "Committing modifications..."
cd $PAGES_BRANCH
git add .
git commit -a -m "gen_manual.sh: Add latest changes."
git push origin gh-pages
cd ..
rm -rf $PAGES_BRANCH



