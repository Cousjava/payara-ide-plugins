#!/bin/sh
export ECLIPSE_VERSION
SWT_VERSION=3.4.0
export SWT_VERSION
ANT_HOME=/export/home/timezra/opt/ant
export ANT_HOME
PATH=$ANT_HOME/bin:$PATH
export PATH
JAVA_HOME=/usr/jdk/jdk1.6.0_10
export JAVA_HOME
SUN_STUDIO_HOME=/opt/SUNWspro
export SUN_STUDIO_HOME
CC_FOR_SWT=$SUN_STUDIO_HOME/bin/cc
export CC_FOR_SWT
CXX_FOR_SWT=$SUN_STUDIO_HOME/bin/CC
export CXX_FOR_SWT
LAUNCHER_VERSION=`find ./plugins/org.eclipse.equinox.launcher.gtk.solaris.sparc -name eclipse_*.so -print | sed "s/^.*eclipse_//" | sed "s/\.so$//"`
export LAUNCHER_VERSION
ASSEMBLY_VERSION=`grep "/org\.eclipse\.swt\.gtk\.solaris\.sparc_" ./assemble.org.eclipse.sdk.solaris.gtk.sparc.xml | sed "s/.*org\.eclipse\.swt\.gtk\.solaris\.sparc_//" | sed "s/\.jar.*//" | sed "s/.*\.//"`
export ASSEMBLY_VERSION
JavaSE16=$JAVA_HOME
export JavaSE16
J2SE15=/usr/jdk/jdk1.5.0_18
export J2SE15
J2SE14=/usr/jdk/j2sdk1.4.2_18
export J2SE14


$JAVA_HOME/bin/java -jar eclipse/plugins/org.eclipse.equinox.launcher_*.jar -data workspace -application org.eclipse.equinox.p2.metadata.generator.EclipseGenerator -flavor tooling -metadataRepositoryName "Solipse" -artifactRepositoryName "Solipse" -metadataRepository "file:eclipse-metadata" -artifactRepository "file:eclipse-metadata" -root "Solipse Platform" -rootVersion ${ECLIPSE_VERSION} -source eclipse -append -publishArtifacts

$JAVA_HOME/bin/java -Declipse.p2.data.area="file:`pwd`/eclipse-provisioned/p2" -jar eclipse/plugins/org.eclipse.equinox.launcher_*.jar -data workspace -application org.eclipse.equinox.p2.director.app.application -flavor tooling -metadataRepository "file:eclipse-metadata" -artifactRepository "file:eclipse-metadata" -installIU "Solipse Platform" -version ${ECLIPSE_VERSION} -p2.os solaris -p2.ws gtk -p2.arch x86 -profile PlatformProfile -profileProperties org.eclipse.update.install.features=true -destination `pwd`/eclipse-provisioned -bundlepool `pwd`/eclipse-provisioned -consoleLog -roaming


