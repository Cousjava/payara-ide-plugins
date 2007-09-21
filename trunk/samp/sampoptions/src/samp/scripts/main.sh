#! /bin/sh

# Usage:
#
#     main.sh [ -R  alternate_root ]
#
#   where:
#     alternate_root is usually passed in from the Solaris installer
#
# Products get installed in <alternate_root>/opt/ so that when Solaris
# is rebooted, they will be in /opt

CMD=$0
PATH_TO_CMD=`/usr/bin/dirname ${CMD}`
LOC=`cd ${PATH_TO_CMD}; pwd`
ROOT_PATH=""
ROOT_OPTION=""

while [ "$1" != "" ]
do
   case $1 in
   -R) ROOT_PATH=$2
       ROOT_OPTION="-R ${ROOT_PATH}"
       shift 2;;
   esac
done


# Absolute path to location where Netbeans should be installed
ST_DIR=${ROOT_PATH}/opt

# Absolute path to SAMP Tooling bundle 
ST_COMPRESSED=${LOC}/xxxxxxxxxxxxx.tar.bz2

# Absolute path to desktop menus
ST_POSTINSTALL=${ROOT_PATH}/usr/share/applications

LOG=/tmp/SAMPToolinginstall.log

/usr/bin/rm -f ${LOG}


echo "" | /usr/bin/tee -a ${LOG}
echo "Checking for existing SAMP Tooling" | /usr/bin/tee -a ${LOG}
if [ -r ${ROOT_PATH}/opt/SAMPTooling ] ; then
   echo "Removing old SAMPTooling" | /usr/bin/tee -a ${LOG}
   /usr/bin/rm -rf ${ROOT_PATH}/opt/SAMPTooling
fi

echo "" | /usr/bin/tee -a ${LOG}
echo "**********************" | /usr/bin/tee -a ${LOG}
echo "***  SAMP Tooling  ***" | /usr/bin/tee -a ${LOG}
echo "**********************" | /usr/bin/tee -a ${LOG}

# Install Netbeans 
echo "Installing SAMP Tooling" | /usr/bin/tee -a ${LOG}

/usr/bin/mkdir -p ${ST_DIR}
###/usr/bin/bzip2 -dc ${ST_COMPRESSED} | (cd ${ST_DIR}; tar xfBp -)

# Run the SAMP Tooling post-install scripts
if [ $usr_ro != 0 ]; then
   echo "WARNING: Cannot install SAMP Tooling GNOME desktop file; ${ROOT_PATH}/usr not writable." | /usr/bin/tee -a ${LOG}
else
   echo "Installing NetBeans GNOME desktop file" | /usr/bin/tee -a ${LOG}
   /usr/bin/mkdir -p ${ST_POSTINSTALL}
   /usr/bin/cp ${LOC}/startmenus/*.desktop ${ST_POSTINSTALL}

# need to add the directory in /etc/xdb/menus/application.menu
fi

echo "SAMP Tooling installation complete." | /usr/bin/tee -a ${LOG}

# reset the GNOME applications menu
if [ "${ROOT_PATH}" = "" ] ; then
   /usr/bin/pkill panel
fi

echo "" | /usr/bin/tee -a ${LOG}
echo "See ${LOG} for a copy of this install output." | /usr/bin/tee -a ${LOG}
echo "" | /usr/bin/tee -a ${LOG}
echo "All done.  This window will exit in 20 seconds." | /usr/bin/tee -a ${LOG}


sleep 20

exit 0

