#!/bin/bash -ex

#NB! It's not entirely local, only the build scripts in Glassfish and Releng projects are local, all plugins, features etc. are still taken from CVS.

# for Glassfish v1.0 use tag T_BRANCH_P2_WIP_IN_BRANCH_ECLIPSE_34_GLASSFISH_V1

#cvs  -d :pserver:anonymous@easyeclipse.cvs.sourceforge.net:/cvsroot/easyeclipse checkout -r BRANCH_ECLIPSE_35_IN_P2_WIP easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
#cp -f patches/download-style.xml easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
#cp -f patches/package.xml easyeclipse/easyeclipse/org.nexb.easyeclipse.releng

mkdir -p easyeclipse/easyeclipse
rsync -avz ../../org.nexb.easyeclipse.releng easyeclipse/easyeclipse/

cp -f build.properties easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
cp -f servicetag-registry.xml easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
cp -f jsf-taglib.jar easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
pushd easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
./build.sh tools-bundle-for-eclipse-local
popd


