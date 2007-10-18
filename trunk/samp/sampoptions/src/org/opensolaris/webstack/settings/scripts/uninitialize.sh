#! /bin/sh

# Usage:
#
#     initialize.sh [ -R  alternate_root ]
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


# Absolute path to location where webstack tooling should be installed
ST_DIR=${ROOT_PATH}/opt

SCRIPTS_DIR=${ROOT_PATH}/opt/webstack/menus



LOG=/tmp/webstacktoolinginitialize.log

/usr/bin/rm -f ${LOG}


echo "" | /usr/bin/tee -a ${LOG}



# UnInstall menus 
echo "UnInstalling WebStack menus" | /usr/bin/tee -a ${LOG}


   echo "UnInstalling NetBeans GNOME desktop file" | /usr/bin/tee -a ${LOG}

   /usr/bin/rm ${HOME}/.local/share/applications/webstack*.desktop

   /usr/bin/rm ${HOME}/.local/share/desktop-directories/webstack*.directory

   /usr/bin/rm   ${HOME}/.config/menus/applications-merged/webstack*.menu




echo "WebStack menus installation complete." | /usr/bin/tee -a ${LOG}

# reset the GNOME applications menu
if [ "${ROOT_PATH}" = "" ] ; then
   /usr/bin/pkill panel
fi

echo "" | /usr/bin/tee -a ${LOG}
echo "See ${LOG} for a copy of this install output." | /usr/bin/tee -a ${LOG}
echo "" | /usr/bin/tee -a ${LOG}
echo "All done.  This window will exit in 3 seconds." | /usr/bin/tee -a ${LOG}


sleep 3

exit 0

