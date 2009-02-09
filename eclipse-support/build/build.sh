#!/bin/bash -ex

cvs  -d :pserver:anonymous@easyeclipse.cvs.sourceforge.net:/cvsroot/easyeclipse checkout -r BRANCH_P2_WIP_IN_BRANCH_ECLIPSE_34 easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
#cp -f patches/download-style.xml easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
#cp -f patches/package.xml easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
cp -f build.properties easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
pushd easyeclipse/easyeclipse/org.nexb.easyeclipse.releng
./build.sh tools-bundle-for-eclipse
popd


