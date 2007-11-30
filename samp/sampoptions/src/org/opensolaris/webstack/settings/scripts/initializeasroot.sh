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
ROOT_PATH=""
USERNAME=$1

echo "making sure that the mysql user and group exists, and owns /var/mysql content"
/usr/sbin/groupadd mysql
/usr/sbin/useradd -g mysql mysql
chown -R mysql:mysql /var/mysql

echo "allowing RW access to httpd.conf, php.ini and my.cnf to user ${USERNAME}"

setfacl -m user:${USERNAME}:rw- /etc/apache2/2.2/httpd.conf
setfacl -m mask:rw- /etc/apache2/2.2/httpd.conf     
setfacl -m user:${USERNAME}:rw- /etc/php5/5.2.4/php.ini 
setfacl -m mask:rw- /etc/php5/5.2.4/php.ini   
setfacl -m user:${USERNAME}:rw- /etc/php5/5.2.4/conf.d/xdebug.ini 
setfacl -m mask:rw- /etc/php5/5.2.4/conf.d/xdebug.ini 
setfacl -m user:${USERNAME}:rw- /etc/mysql/5.0/my.cnf 
setfacl -m mask:rw- /etc/mysql/5.0/my.cnf 
setfacl -m user:${USERNAME}:rwx /var/apache2/2.2/htdocs 
setfacl -m mask:rwx  /var/apache2/2.2/htdocs

A=`fgrep solaris.smf.manage.mysql /etc/security/prof_attr`
if [ "$?" -ne "0"  ]; then
	echo "MySql 5 Administration::::auths=solaris.smf.value.mysql,solaris.smf.manage.mysql" >>/etc/security/prof_attr
else
        echo "mysql smf profile already exists"

fi

B=`fgrep solaris.smf.manage.mysql /etc/security/auth_attr`
if [ "$?" -ne "0"  ]; then
	echo 'solaris.smf.manage.mysql:::webstack::' >>/etc/security/auth_attr
else
        echo "mysql smf auth already exists"

fi

svccfg -s mysql setprop general/action_authorization=astring: 'solaris.smf.manage.mysql'
svccfg -s mysql setprop general/value_authorization=astring:  'solaris.smf.manage.mysql'
svcadm refresh mysql
echo "adding apache22 and mysql privileges to user ${USERNAME}"
usermod -P 'Apache 22 Administration','MySql 5 Administration' ${USERNAME}
 
exit 0
