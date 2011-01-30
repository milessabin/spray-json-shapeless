#!bash


# Clone and update testing projects..

PROJ=sbt-deps
rm -rf $PROJ
git clone https://github.com/hdeshev/${PROJ}.git
cd ${PROJ}
sbt update
read -d '' CONFIG <<EOF
(
:project-package "${PROJ}"
:use-sbt t
:sbt-subprojects (
  (:name "web" :deps ("core"))
  (:name "core" :deps ())
  )
)
EOF
echo $CONFIG > .ensime
cd ..

