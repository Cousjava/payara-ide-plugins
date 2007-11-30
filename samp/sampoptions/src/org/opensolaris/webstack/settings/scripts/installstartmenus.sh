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

# Absolute path to WebStack Tooling bundle 
ST_COMPRESSED=${LOC}/xxxxxxxxxxxxx.tar.bz2

# Absolute path to desktop menus
ST_POSTINSTALL=${ROOT_PATH}/usr/share/applications

LOG=/tmp/webstacktoolinginstall.log

/usr/bin/rm -f ${LOG}


echo "" | /usr/bin/tee -a ${LOG}
echo "Checking for existing WebStack Tooling" | /usr/bin/tee -a ${LOG}
if [ -r ${ROOT_PATH}/opt/WebStackTooling ] ; then
   echo "Removing old WebStack Tooling" | /usr/bin/tee -a ${LOG}
   /usr/bin/rm -rf ${ROOT_PATH}/opt/webstacktooling
fi

echo "" | /usr/bin/tee -a ${LOG}
echo "**********************" | /usr/bin/tee -a ${LOG}
echo "***  WebStack Tooling  ***" | /usr/bin/tee -a ${LOG}
echo "**********************" | /usr/bin/tee -a ${LOG}

# Install Netbeans 
echo "Installing WebStack Tooling" | /usr/bin/tee -a ${LOG}

/usr/bin/mkdir -p ${ST_DIR}
###/usr/bin/bzip2 -dc ${ST_COMPRESSED} | (cd ${ST_DIR}; tar xfBp -)

# Run the WebStack Tooling post-install scripts
if [ $usr_ro != 0 ]; then
   echo "WARNING: Cannot install WebStack Tooling GNOME desktop file; ${ROOT_PATH}/usr not writable." | /usr/bin/tee -a ${LOG}
else
   echo "Installing NetBeans GNOME desktop file" | /usr/bin/tee -a ${LOG}
   /usr/bin/mkdir -p ${ST_POSTINSTALL}
   /usr/bin/cp ${LOC}/startmenus/*.desktop ${ST_POSTINSTALL}

# need to add the directory in /etc/xdb/menus/application.menu
fi

echo "WebStack Tooling installation complete." | /usr/bin/tee -a ${LOG}

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

