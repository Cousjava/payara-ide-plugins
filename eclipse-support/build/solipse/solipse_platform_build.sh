#!/bin/sh

# #######################################################
# This script makes assumptions about your JAVA_HOMEs,
# ANT_HOME and your SunStudio home.  Please replace these 
# values in the environment variable declarations below.
# Also, on Nexenta, use gcc as your CC_FOR_SWT and GCC
# as your CXX_FOR_SWT.
# #######################################################

# unzip -q eclipse-sourceBuild-srcIncluded-3.4.2.zip

# #######################################################
# set environment variables
# #######################################################
ECLIPSE_VERSION=3.4.2
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

# #######################################################
# set the build.properties
# #######################################################
perl -pi -e 's~J2SE-1\.4=~J2SE-1\.4=$ENV{J2SE14}/jre/lib/rt.jar:$ENV{J2SE14}/jre/lib/jsse.jar:$ENV{J2SE14}/jre/lib/jce.jar~' ./build.properties
perl -pi -e 's~J2SE-1\.5=~J2SE-1\.5=$ENV{J2SE15}/jre/lib/rt.jar:$ENV{J2SE15}/jre/lib/jsse.jar:$ENV{J2SE15}/jre/lib/jce.jar~' ./build.properties
perl -pi -e 's~JavaSE-1\.6=~JavaSE-1\.6=$ENV{JavaSE16}/jre/lib/rt.jar:$ENV{JavaSE16}/jre/lib/jsse.jar:$ENV{JavaSE16}/jre/lib/jce.jar:$ENV{JavaSE16}/jre/lib/resources.jar~' ./build.properties

# #######################################################
# alter the build feature
# #######################################################

perl -pi -e 's~<target name="rootFilessolaris_gtk_x86">~<target name="rootFilessolaris_gtk_x86">\n\t\t<mkdir dir="\$\{feature.base\}/solaris.gtk.x86/\$\{collectingFolder\}/"/>\n\t\t<copy todir="\$\{feature.base\}/solaris.gtk.x86/\$\{collectingFolder\}/" failonerror="true" overwrite="true">\n\t\t\t<fileset dir="\$\{basedir\}/gtk">\n\t\t\t\t<include name="**"/>\n\t\t\t</fileset>\n\t\t</copy>~' ./features/org.eclipse.sdk/build.xml

# #######################################################
# alter the swt build scripts
# #######################################################

# this is a big change from eclipse 3.3 to 3.4
perl -pi -e 's~<!-- unzip swt zips -->~<!-- unzip swt zips -->\n\t\t<ant antfile="\$\{buildDirectory\}/plugins/org.eclipse.swt.\$\{installWs\}.\$\{installOs\}.\$\{installArch\}/build.xml" target="src.zip" dir="\$\{buildDirectory\}/plugins/org.eclipse.swt.\$\{installWs\}.\$\{installOs\}.\$\{installArch\}" />~' ./build.xml

perl -pi -e 's~<fileset dir="\$\{buildDirectory\}/plugins/org.eclipse.rcp.source.\$\{installOs\}.\$\{installWs\}.\$\{installArch\}/src/">~<fileset dir="\$\{buildDirectory\}/plugins/org.eclipse.swt.\$\{installWs\}.\$\{installOs\}.\$\{installArch\}" >~' ./build.xml

perl -pi -e 's~<include name="org.eclipse.swt.\$\{installWs\}.\$\{installOs\}.\$\{installArch\}_\*/\*.zip" />~<include name="src.zip" />~' ./build.xml

perl -pi -e 's~all: make_swt make_atk make_awt make_glx make_cde~all: make_swt make_atk make_awt~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

perl -pi -e 's~make -f \$MAKEFILE all \$MAKE_GNOME \$MAKE_CAIRO \$MAKE_AWT \$MAKE_MOZILLA~make -f \$MAKEFILE all \$MAKE_GNOME \$MAKE_CAIRO \$MAKE_AWT~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/build.sh

# add xulrunner support
# perl -pi -e 's~MAKE_MOZILLA=make_mozilla~XULRUNNER_SDK=/export/home/timezra/Documents/unzipped/xulrunner/mozilla/xulrunner-build/dist/sdk\n\t\texport XULRUNNER_SDK\n\t\tXULRUNNER_INCLUDES="-I\$\{XULRUNNER_SDK\}/include"\n\t\tXULRUNNER_LIBS="-L\$\{XULRUNNER_SDK\}/lib -lxpcomglue"\n\t\texport XULRUNNER_INCLUDES\n\t\texport XULRUNNER_LIBS\n\t\texport MOZILLA_INCLUDES\n\t\texport MOZILLA_LIBS\n\t\tMAKE_MOZILLA=make_mozilla\n\t\tMAKE_XULRUNNER=make_xulrunner\n\t\tMAKE_XPCOMINIT=make_xpcominit~g' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/build.sh

# perl -pi -e 's~make -f \$MAKEFILE all \$MAKE_GNOME \$MAKE_CAIRO \$MAKE_AWT \$MAKE_MOZILLA~make -f \$MAKEFILE all \$MAKE_GNOME \$MAKE_CAIRO \$MAKE_AWT \$MAKE_MOZILLA \$MAKE_XULRUNNER \$MAKE_XPCOMINIT~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/build.sh

# perl -pi -e 's~MOZILLA_PREFIX\s*=\s*swt-mozilla~MOZILLA_PREFIX = swt-mozilla\nXULRUNNER_PREFIX = swt-xulrunner\nXPCOMINIT_PREFIX = swt-xpcominit~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~MOZILLA_LIB\s*=\s*lib\$\(MOZILLA_PREFIX\)-\$\(WS_PREFIX\)-\$\(SWT_VERSION\)\.so~MOZILLA_LIB = lib\$\(MOZILLA_PREFIX\)-\$\(WS_PREFIX\)-\$\(SWT_VERSION\)\.so\nXULRUNNER_LIB = lib\$\(XULRUNNER_PREFIX\)-\$\(WS_PREFIX\)-\$\(SWT_VERSION\)\.so\nXPCOMINIT_LIB = lib\$\(XPCOMINIT_PREFIX\)-\$\(WS_PREFIX\)-\$\(SWT_VERSION\)\.so~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~MOZILLA_OBJECTS\s*=\s*swt\.o xpcom\.o xpcom_custom\.o xpcom_structs\.o xpcom_stats\.o~MOZILLA_OBJECTS = swt\.o xpcom\.o xpcom_custom\.o xpcom_structs\.o xpcom_stats\.o\nXULRUNNER_OBJECTS = swt\.o xpcomxul\.o xpcomxul_custom\.o xpcomxul_structs\.o xpcomxul_stats\.o\nXPCOMINIT_OBJECTS = swt\.o xpcominit\.o xpcominit_structs\.o xpcominit_stats\.o~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~\$\(CC\) \$\(CFLAGS\) \$\(GLXCFLAGS\) -c glx_stats\.c~\$\(CC\) \$\(CFLAGS\) \$\(GLXCFLAGS\) -c glx_stats\.c\n\n#\n# XULRunner lib\n#\nmake_xulrunner:\$\(XULRUNNER_LIB\)\n\n\$\(XULRUNNER_LIB\): \$\(XULRUNNER_OBJECTS\)\n\t\$\(CXX\) -o \$\(XULRUNNER_LIB\) \$\(XULRUNNER_OBJECTS\) \$\(MOZILLALIBS\) \$\{XULRUNNER_LIBS\}\n\nxpcomxul\.o: xpcom\.cpp\n\t\$\(CXX\) -o xpcomxul\.o \$\(MOZILLACFLAGS\) \$\{XULRUNNER_INCLUDES\} -c xpcom\.cpp\n\nxpcomxul_structs\.o: xpcom_structs\.cpp\n\t\$\(CXX\) -o xpcomxul_structs\.o \$\(MOZILLACFLAGS\) \$\{XULRUNNER_INCLUDES\} -c xpcom_structs\.cpp\n\nxpcomxul_custom\.o: xpcom_custom\.cpp\n\t\$\(CXX\) -o xpcomxul_custom\.o \$\(MOZILLACFLAGS\) \$\{XULRUNNER_INCLUDES\} -c xpcom_custom\.cpp\n\nxpcomxul_stats\.o: xpcom_stats\.cpp\n\t\$\(CXX\) -o xpcomxul_stats\.o \$\(MOZILLACFLAGS\) \$\{XULRUNNER_INCLUDES\} -c xpcom_stats\.cpp\n\n#\n# XPCOMInit lib\n#\nmake_xpcominit:\$\(XPCOMINIT_LIB\)\n\n\$\(XPCOMINIT_LIB\): \$\(XPCOMINIT_OBJECTS\)\n\t\$\(CXX\) -o \$\(XPCOMINIT_LIB\) \$\(XPCOMINIT_OBJECTS\) \$\(MOZILLALIBS\) \$\{XULRUNNER_LIBS\}\n\nxpcominit\.o: xpcominit\.cpp\n\t\$\(CXX\) \$\(MOZILLACFLAGS\) \$\{XULRUNNER_INCLUDES\} -c xpcominit\.cpp\n\nxpcominit_structs\.o: xpcominit_structs\.cpp\n\t\$\(CXX\) \$\(MOZILLACFLAGS\) \$\{XULRUNNER_INCLUDES\} -c xpcominit_structs\.cpp\n\nxpcominit_stats\.o: xpcominit_stats\.cpp\n\t\$\(CXX\) \$\(MOZILLACFLAGS\) \$\{XULRUNNER_INCLUDES\} -c xpcominit_stats\.cpp~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# set environment variables
perl -pi -e 's~SWT_VERSION=\$\(maj_ver\)\$\(min_ver\)~SWT_VERSION=\$\(maj_ver\)\$\(min_ver\)\nJAVA_HOME=$ENV{JAVA_HOME}\nCC=$ENV{CC_FOR_SWT}\nCXX=$ENV{CXX_FOR_SWT}\n~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# completely replace the MOZILLACFLAGS and MOZILLALIBS
# perl -pi -e 's~\s+-DMOZILLA_STRICT_API=1\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-fno-rtti\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-fno-exceptions\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-Wall\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-DSWT_VERSION=\$\(SWT_VERSION\) \$\(NATIVE_STATS\)\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-Wno-non-virtual-dtor\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-fPIC\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-I\.\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-I\$\(MOZILLA_SDK\)\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-include \$\(MOZILLA_SDK\)/mozilla-config\.h\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-I\$\(MOZILLA_SDK\)/nspr/include\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-I\$\(MOZILLA_SDK\)/xpcom/include\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-I\$\(MOZILLA_SDK\)/string/include\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-I\$\(MOZILLA_SDK\)/embed_base/include\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-I\$\(MOZILLA_SDK\)/embedstring/include~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~MOZILLALIBS\s+=\s+-G -s -Wl,--version-script=mozilla_exports -Bsymbolic\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-L\$\(MOZILLA_SDK\)/embedstring/bin -lembedstring\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-L\$\(MOZILLA_SDK\)/embed_base/bin -lembed_base_s\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-L\$\(MOZILLA_SDK\)/xpcom/bin -lxpcomglue_s -lxpcom\s+\\~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak
# perl -pi -e 's~\s+-L\$\(MOZILLA_SDK\)/nspr/bin -lnspr4 -lplds4 -lplc4~~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~MOZILLACFLAGS\s+=\s+-O\s+\\~MOZILLACFLAGS = -O \\\n\t-DSWT_VERSION=\$\(SWT_VERSION\) \\\n\t\$\(NATIVE_STATS\) \\\n\t-DMOZILLA_STRICT_API=1 \\\n\t-I\. \\\n\t-I\$\(JAVA_HOME\)/include \\\n\t-I\$\(JAVA_HOME\)/include/solaris \\\n\t\$\{SWT_PTR_CFLAGS\}\nMOZILLALIBS = -G -KPIC -s\nMOZILLAEXCLUDES = -DNO_XPCOMGlueShutdown -DNO_XPCOMGlueStartup~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# fix the CXX arguments for mozilla bridge compilation
# perl -pi -e 's~\$\(CXX\) -o \$\(MOZILLA_LIB\) \$\(MOZILLA_OBJECTS\) \$\(MOZILLALIBS\)~\$\(CXX\) -o \$\(MOZILLA_LIB\) \$\(MOZILLA_OBJECTS\) \$\(MOZILLALIBS\) \$\{MOZILLA_LIBS\}~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~\$\(CXX\) \$\(MOZILLACFLAGS\) -c xpcom\.cpp~\$\(CXX\) \$\(MOZILLACFLAGS\) \$\(MOZILLAEXCLUDES\) \$\{MOZILLA_INCLUDES\} -c xpcom\.cpp~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~\$\(CXX\) \$\(MOZILLACFLAGS\) -c xpcom_structs\.cpp~\$\(CXX\) \$\(MOZILLACFLAGS\) \$\{MOZILLA_INCLUDES\} -c xpcom_structs\.cpp~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~\$\(CXX\) \$\(MOZILLACFLAGS\) -c xpcom_custom\.cpp~\$\(CXX\) \$\(MOZILLACFLAGS\) \$\{MOZILLA_INCLUDES\} -c xpcom_custom\.cpp~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# perl -pi -e 's~\$\(CXX\) \$\(MOZILLACFLAGS\) -c xpcom_stats\.cpp~\$\(CXX\) \$\(MOZILLACFLAGS\) \$\{MOZILLA_INCLUDES\} -c xpcom_stats\.cpp~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ PI/gtk/library/make_solaris.mak

# fix the MozillaDelegate.java class to allow mozilla for solaris
# perl -pi -e 's~IsLinux\s*=\s*osName\.startsWith\s*\("linux"\);\s+//\$NON-NLS-1\$~IsLinux = osName\.startsWith \("linux"\) || osName\.startsWith \("sunos"\); //\$NON-NLS-1\$ //\$NON-NLS-2\$~' ./plugins/org.eclipse.swt/Eclipse\ SWT\ Mozilla/gtk/org/eclipse/swt/browser/MozillaDelegate.java

# perl -pi -e 's~IsLinux~IsMozillaSupported~g' ./plugins/org.eclipse.swt/Eclipse\ SWT\ Mozilla/gtk/org/eclipse/swt/browser/MozillaDelegate.java

perl -pi -e 's~fileset dir="\$\{plugindir\}/Eclipse SWT Program/cde/library/"~fileset dir="\$\{plugindir\}/Eclipse SWT PI/common/library/"~' ./plugins/org.eclipse.swt.gtk.solaris.x86/build.xml

# #######################################################
# alter the launcher build scripts
# #######################################################

perl -pi -e 's~org.eclipse.equinox.executable/contributed/\$\{installWs\}/\$\{installOs\}/.\$\{installArch\}/" />~org.eclipse.equinox.executable/contributed/\$\{installWs\}/\$\{installOs\}/\$\{installArch\}/" />~' ./build.xml

perl -pi -e 's~<fileset dir="\$\{basedir\}/../../launchertmp">~<fileset dir="\$\{basedir\}/../../features/org.eclipse.equinox.executable/contributed/gtk/solaris/x86">~' ./features/org.eclipse.platform/build.xml

perl -pi -e 's~#\s*JAVA_HOME\s*-\s*JAVA_HOME for JNI headers~JAVA_HOME=$ENV{JAVA_HOME}\nCC=$ENV{SUN_STUDIO_HOME}/bin/cc\n~' ./features/org.eclipse.equinox.executable/library/gtk/make_solaris.mak

rm -r ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86

cp -r ./plugins/org.eclipse.equinox.launcher.gtk.solaris.sparc ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86

mv ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/launcher.gtk.solaris.sparc.properties ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/launcher.gtk.solaris.x86.properties

perl -pi -e 's~sparc~x86~g' ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/build.xml ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/build.properties ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/launcher.gtk.solaris.x86.properties ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/META-INF/MANIFEST.MF

perl -pi -e 's~Sparc~X86~g' ./plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/launcher.gtk.solaris.x86.properties

cp -r ./plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.sparc ./plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.x86

mv ./plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.x86/launcher.gtk.solaris.sparc.properties ./plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.x86/launcher.gtk.solaris.x86.properties

perl -pi -e 's~sparc~x86~g' ./plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.x86/build.properties ./plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.x86/META-INF/MANIFEST.MF

perl -pi -e 's~Sparc~X86~g' ./plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.x86/launcher.gtk.solaris.x86.properties

# rebuild the launchersrc.zip

mkdir launchertmp

mv ./plugins/org.eclipse.platform/launchersrc.zip launchertmp

cd launchertmp

unzip -q launchersrc.zip

perl -pi -e 's~#\s*JAVA_HOME\s*-\s*JAVA_HOME for JNI headers~JAVA_HOME=$ENV{JAVA_HOME}\nCC=$ENV{SUN_STUDIO_HOME}/bin/cc\n~' ./library/gtk/make_solaris.mak

cd library/gtk

sh build.sh

cp eclipse_*.so ../../../plugins/org.eclipse.equinox.launcher.gtk.solaris.x86/eclipse_$LAUNCHER_VERSION.so

cp eclipse_*.so ../../../plugins/org.eclipse.equinox.launcher/fragments/org.eclipse.equinox.launcher.gtk.solaris.x86/eclipse_$LAUNCHER_VERSION.so

cd ../..

rm launchersrc.zip

zip -q -r launchersrc.zip *

mv launchersrc.zip ../plugins/org.eclipse.platform

cd ..

rm -r launchertmp

# #######################################################
# alter the filesystem build scripts
# #######################################################

perl -pi -e 's~plugins/org.eclipse.core.filesystem/natives/unix/linux~plugins/org.eclipse.core.filesystem/natives/unix/\$\{installOs\}~' ./build.xml

perl -pi -e 's~plugins/org.eclipse.core.filesystem.linux.\$\{installArch\}/os/linux/\$\{installArch\}~plugins/org.eclipse.core.filesystem.\$\{installOs\}.\$\{installArch\}/os/\$\{installOs\}/\$\{installArch\}~' ./build.xml

perl -pi -e 's~JAVA_HOME\s*=\s*\~/vm/sun142~JAVA_HOME=$ENV{JAVA_HOME}\nCC=$ENV{SUN_STUDIO_HOME}/bin/cc\n~' ./plugins/org.eclipse.core.filesystem/natives/unix/solaris/Makefile

perl -pi -e 's~sparc~x86~g' ./plugins/org.eclipse.core.filesystem/natives/unix/solaris/Makefile

cp -r ./plugins/org.eclipse.core.filesystem.solaris.sparc ./plugins/org.eclipse.core.filesystem.solaris.x86

mv ./plugins/org.eclipse.core.filesystem.solaris.x86/os/solaris/sparc ./plugins/org.eclipse.core.filesystem.solaris.x86/os/solaris/x86

cd ./plugins/org.eclipse.core.filesystem/natives/unix/solaris

make

cp liblocalfile_1_0_0.so ../../../../org.eclipse.core.filesystem.solaris.x86/os/solaris/x86

cd ../../../../..

perl -pi -e 's~sparc~x86~g' ./plugins/org.eclipse.core.filesystem.solaris.x86/build.xml ./plugins/org.eclipse.core.filesystem.solaris.x86/META-INF/MANIFEST.MF ./plugins/org.eclipse.core.filesystem.solaris.x86/BUILD_INFO.txt

perl -pi -e 's~Sparc~X86~g' ./plugins/org.eclipse.core.filesystem.solaris.x86/fragment.properties

perl -pi -e 's~make install JAVA_HOME=\$TOOLSRV/jdk/1.5.0_08/SunOS/5.9~make install JAVA_HOME=$ENV{JAVA_HOME}~' ./plugins/org.eclipse.core.filesystem.solaris.x86/BUILD_INFO.txt

perl -pi -e 's~setenv PATH \$TOOLSRV/gcc/3.4.4/SunOS/5.5/bin:\$PATH~setenv PATH $ENV{SUN_STUDIO_HOME}/bin:\$PATH~' ./plugins/org.eclipse.core.filesystem.solaris.x86/BUILD_INFO.txt

# #######################################################
# alter the update build scripts
# #######################################################

perl -pi -e 's~plugins/org.eclipse.update.core.linux/src~plugins/org.eclipse.update.core.\$\{installOs\}/src~' ./build.xml

cp -r ./plugins/org.eclipse.update.core.linux ./plugins/org.eclipse.update.core.solaris

mv ./plugins/org.eclipse.update.core.solaris/os/linux ./plugins/org.eclipse.update.core.solaris/os/solaris

perl -pi -e 's~linux~solaris~g' ./plugins/org.eclipse.update.core.solaris/build.xml ./plugins/org.eclipse.update.core.solaris/src/build.xml ./plugins/org.eclipse.update.core.solaris/META-INF/MANIFEST.MF

perl -pi -e 's~Linux~Solaris~g' ./plugins/org.eclipse.update.core.solaris/META-INF/MANIFEST.MF

perl -pi -e 's~gcc\s*-o\s*\$\{library-file\}\s*-shared\s*-I\$\{src-path\}\s*-I\$\{header-solaris-path\}\s*-fPIC\s*\$\{library-file\}~$ENV{SUN_STUDIO_HOME}/bin/cc -o \$\{library-file\} -G -I\$\{src-path\} -I\$\{header-path\} -I\$\{header-solaris-path\} -KPIC \$\{src-path\}/*.c~' ./plugins/org.eclipse.update.core.solaris/src/build.xml

perl -pi -e 's~apply executable="gcc"~apply executable="$ENV{SUN_STUDIO_HOME}/bin/cc"~' ./plugins/org.eclipse.update.core.solaris/src/build.xml

perl -pi -e 's~arg value="-shared"~arg value="-G"~' ./plugins/org.eclipse.update.core.solaris/src/build.xml

perl -pi -e 's~arg value="-fPIC"~arg value="-KPIC"~' ./plugins/org.eclipse.update.core.solaris/src/build.xml

perl -pi -e 's~statfs~statvfs~' ./plugins/org.eclipse.update.core.solaris/src/update.c

# #######################################################
#  use new platform assemble and package
# #######################################################

#platform# - this is allready changed in ...platform....xml

# perl -pi -e 's~org\.eclipse\.swt\.gtk\.solaris\.x86\.source_\d+\.\d+\.\d+\.$ENV{ASSEMBLY_VERSION}~org.eclipse.swt.gtk.solaris.x86.source_$ENV{SWT_VERSION}.HEAD~g' assemble.org.eclipse.sdk.solaris.gtk.x86.xml

# perl -pi -e 's~org\.eclipse\.swt\.gtk\.solaris\.x86_\d+\.\d+\.\d+\.$ENV{ASSEMBLY_VERSION}~org.eclipse.swt.gtk.solaris.x86_$ENV{SWT_VERSION}.HEAD~g' assemble.org.eclipse.sdk.solaris.gtk.x86.xml

# #######################################################
# Alter the doc building script to overwrite when the
# executable is unzipped.
# #######################################################

perl -pi -e 's~<arg line="-qq \$\{buildDirectory\}/\$\{buildLabel\}/\*.zip" />~<arg line="-qq -o \$\{buildDirectory\}/\$\{buildLabel\}/\*.zip" />~g' ./build.xml


#platform# We don't care about PDE
# #######################################################
# Fix compilation errors in 3.4RC2.
# #######################################################
#perl -pi -e 's~return \(\(String\) entry1.getKey\(\)\).compareTo\(entry2.getKey\(\)\);~return \(\(String\) entry1.getKey\(\)\).compareTo\(\(String\) entry2.getKey\(\)\);~g' ./plugins/org.eclipse.pde.api.tools/src_ant/org/eclipse/pde/api/tools/internal/tasks/#APIToolsVerificationTask.java
#
#perl -pi -e 's~return \(\(String\) entry1.getKey\(\)\).compareTo\(entry2.getKey\(\)\);~return \(\(String\) entry1.getKey\(\)\).compareTo\(\(String\) entry2.getKey\(\)\);~g' ./plugins/org.eclipse.pde.api.tools/src_ant/org/eclipse/pde/api/tools/internal/tasks/#DeltaReportConversionTask.java
#
#perl -pi -e 's~return \(\(String\)o1\).compareTo\(o2\);~return \(\(String\)o1\).compareTo\(\(String\)o2\);~g' ./plugins/org.eclipse.pde.api.tools.ui/src/org/eclipse/pde/api/tools/ui/internal/wizards/ApiProfileWizardPage.java
#
# #######################################################
# New in Eclipse 3.4.1 -- the classpath for osgi 
#  compilation does not work.
# #######################################################
perl -pi -e 's~<property name="bundleBootClasspath" value="osgi/exceptions\.jar;osgi/xmlParserAPIs\.jar;\$\{CDC-1\.0/Foundation-1\.0\}"/>~<condition property="dir_bootclasspath" value="\$\{java.home\}/\.\./Classes">\n\t\t<os family="mac"/>\n\t</condition>\n\t<property name="dir_bootclasspath" value="\$\{java\.home\}/lib"/>\n\t<path id="path_bootclasspath">\n\t\t<fileset dir="\$\{dir_bootclasspath\}">\n\t\t\t<include name="\*\.jar"/>\n\t\t</fileset>\n\t\t<fileset dir="osgi">\n\t\t\t<include name="exceptions\.jar"/>\n\t\t\t<include name="xmlParserAPIs\.jar"/>\n\t\t</fileset>\n\t</path>\n\t<property name="bundleBootClasspath" refid="path_bootclasspath"/>~' ./plugins/org.eclipse.osgi/build.xml

# #######################################################
# Build Eclipse and Compile the Native Libraries.
# #######################################################

./build -os solaris -ws gtk -arch x86 -compilelibs

# #######################################################
# Provision the eclipse distribution for p2 updates
# #######################################################

# $JAVA_HOME/bin/java -jar eclipse/plugins/org.eclipse.equinox.launcher_*.jar -data workspace -application org.eclipse.equinox.p2.metadata.generator.EclipseGenerator -flavor tooling -metadataRepositoryName "Solipse" -artifactRepositoryName "Solipse" -metadataRepository "file:eclipse-metadata" -artifactRepository "file:eclipse-metadata" -root "Solipse Platform" -rootVersion ${ECLIPSE_VERSION} -source eclipse -append -publishArtifacts

# $JAVA_HOME/bin/java -Declipse.p2.data.area="file:`pwd`/eclipse-provisioned/p2" -jar eclipse/plugins/org.eclipse.equinox.launcher_*.jar -data workspace -application org.eclipse.equinox.p2.director.app.application -flavor tooling -metadataRepository "file:eclipse-metadata" -artifactRepository "file:eclipse-metadata" -installIU "Solipse Platform" -version ${ECLIPSE_VERSION} -p2.os solaris -p2.ws gtk -p2.arch x86 -profile PlatformProfile -profileProperties org.eclipse.update.install.features=true -destination `pwd`/eclipse-provisioned -bundlepool `pwd`/eclipse-provisioned -consoleLog -roaming


