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

FILE="/var/apache2/2.2/htdocs/phpsample"

if test -d $FILE
then
       echo "PHP sample already installed "
else
        echo "installing the PHP samples under $FILE"
        unzip /opt/webstack/sample/phpsample.zip -d /var/apache2/2.2/htdocs
        unzip /opt/webstack/sample/jmaki-solaris-express-1.0.zip -d /var/apache2/2.2/htdocs/phpsample/jmaki
   
fi

echo "Press the enter key to close this dialog."
read answer
echo ""
exit 0
