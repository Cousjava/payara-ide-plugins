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

SCRIPTS_DIR=/opt/webstack/menus
echo "Installing global menus for the WebStack"
/usr/bin/mkdir -p ${HOME}/.local/share/applications
/usr/bin/cp  ${SCRIPTS_DIR}/.local/share/applications/*.desktop ${HOME}/.local/share/applications

/usr/bin/mkdir -p ${HOME}/.local/share/desktop-directories
/usr/bin/cp ${SCRIPTS_DIR}/.local/share/desktop-directories/*.directory ${HOME}/.local/share/desktop-directories
/usr/bin/mkdir -p ${HOME}/.config/menus/applications-merged
/usr/bin/cp  ${SCRIPTS_DIR}/.config/menus/applications-merged/*.menu ${HOME}/.config/menus/applications-merged

/usr/bin/pkill panel
CURRENTUSER=${USER}
if [ ${CURRENTUSER} = root ]; then
echo "root user is preconfigured."
else

echo " The installation of the Web Stack needs to run a script as the root user."
echo " This script will add the Apache2 and Mysql SMF privileges and modify the ACL of the apache and   PHP configuration files."
echo " Please enter the root password in order to complete the installation of the Web Stack for user ${CURRENTUSER}, or CTRL-C to stop." 

su - root -c "/opt/webstack/bin/initializeasroot.sh ${CURRENTUSER}"
fi

echo "All done.  Press ENTER to continue." 
read answer
echo ""

exit 0
