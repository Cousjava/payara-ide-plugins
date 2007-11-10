#! /bin/sh

ROOT_PATH=""


# Absolute path to location where webstack tooling should be installed
ST_DIR=${ROOT_PATH}/opt

SCRIPTS_DIR=${ROOT_PATH}/opt/webstack/menus
echo "Installing global menus for the WebStack"
/usr/bin/mkdir -p ${HOME}/.local/share/applications
/usr/bin/cp  ${SCRIPTS_DIR}/.local/share/applications/*.desktop ${HOME}/.local/share/applications

/usr/bin/mkdir -p ${HOME}/.local/share/desktop-directories
/usr/bin/cp ${SCRIPTS_DIR}/.local/share/desktop-directories/*.directory ${HOME}/.local/share/desktop-directories
/usr/bin/mkdir -p ${HOME}/.config/menus/applications-merged
/usr/bin/cp  ${SCRIPTS_DIR}/.config/menus/applications-merged/*.menu ${HOME}/.config/menus/applications-merged

/usr/bin/pkill panel
ME=${USER}
if [ ${ME} = root ]; then
echo "nothing to do, root can do anything!"
else

echo " Please enter the root password in order to complete the installation of the WebStack by user ${ME}" 

su - root -c "${ROOT_PATH}/opt/webstack/bin/initializeasroot.sh ${ME}"
fi

echo "All done.  This window will exit in 5 seconds." 

sleep 5

exit 0
