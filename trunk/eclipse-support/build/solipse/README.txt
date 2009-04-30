To create a solaris x86 Eclipse Platform build, you need:

 * follow http://code.google.com/p/solipse/wiki/BasicHowToBuild steps 1 - 6

 * copy/overwrite following files in solipse build dir: 
	* assemble.org.eclipse.platform.solaris.gtk.x86.xml
	* build.xml
	* package.org.eclipse.platform.solaris.gtk.x86.xml
	* solipse1.sh
	* solipse_platform_build.sh

 * execute solipse_platform_build.sh

 * after this is finished, copy org.eclipse.platform.doc.user_*.jar to plugins jar in eclipse directory in solipse build directory. (you might have to download some other platform jar from eclipse site. This is a workaround to a problem that I hope to fix)

 * execute solipse1.sh
