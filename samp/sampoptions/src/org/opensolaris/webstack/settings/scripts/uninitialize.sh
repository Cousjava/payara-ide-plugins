#! /bin/sh

#
#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the
# Common Development and Distribution License (the "License").
# You may not use this file except in compliance with the License.
#
# You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
# or http://www.opensolaris.org/os/licensing.
# See the License for the specific language governing permissions
# and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each
# file and include the License file at usr/src/OPENSOLARIS.LICENSE.
# If applicable, add the following below this CDDL HEADER, with the
# fields enclosed by brackets "[]" replaced with your own identifying
# information: Portions Copyright [yyyy] [name of copyright owner]
#
# CDDL HEADER END
#
# Copyright 2007 Sun Microsystems, Inc.  All rights reserved.
# Use is subject to license terms.
#
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

