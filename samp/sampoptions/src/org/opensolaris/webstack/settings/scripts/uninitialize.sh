#! /bin/sh


CMD=$0
PATH_TO_CMD=`/usr/bin/dirname ${CMD}`
LOC=`cd ${PATH_TO_CMD}; pwd`
ROOT_PATH=""

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

# UnInstall menus 
echo "Un-installing WebStack menus" 


/usr/bin/rm ${HOME}/.local/share/applications/webstack*.desktop
/usr/bin/rm ${HOME}/.local/share/desktop-directories/webstack*.directory
/usr/bin/rm   ${HOME}/.config/menus/applications-merged/webstack*.menu

echo "WebStack menus un-installation complete."}



echo "WebStack menus installation complete." | /usr/bin/tee -a ${LOG}

# reset the GNOME applications menu
if [ "${ROOT_PATH}" = "" ] ; then
   /usr/bin/pkill panel
fi

echo "All done.  This window will exit in 3 seconds." 

sleep 3

exit 0

