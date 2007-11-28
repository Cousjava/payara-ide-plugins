#! /bin/sh

ROOT_PATH=""
USERNAME=$1

echo "adding apache22 and mysql privileges to user ${USERNAME}"
usermod -P 'Apache 22 Administration','MySql 5 Administration' ${USERNAME}

echo "allowing RW access to httpd.conf, php.ini and my.cnf to user ${USERNAME}"

setfacl -m user:${USERNAME}:rw- /etc/apache2/2.2/httpd.conf
setfacl -m mask:rw- /etc/apache2/2.2/httpd.conf     
setfacl -m user:${USERNAME}:rw- /etc/php5/5.2.4/php.ini 
setfacl -m mask:rw- /etc/php5/5.2.4/php.ini   
setfacl -m user:${USERNAME}:rw- /etc/mysql/5.0/my.cnf 
setfacl -m mask:rw- /etc/mysql/5.0/my.cnf 
setfacl -m user:${USERNAME}:rwx /var/apache2/2.2/htdocs 
setfacl -m mask:rwx  /var/apache2/2.2/htdocs

A=`fgrep solaris.smf.manage.mysql /etc/security/prof_attr`
if [ "$?" -ne "0"  ]; then
	echo "MySql 5 Administration::::auths=solaris.smf.value.mysql,solaris.smf.manage.mysql" >>/etc/security/prof_attr
else
        echo "mysql smf profile already done"

fi

B=`fgrep solaris.smf.manage.mysql /etc/security/auth_attr`
if [ "$?" -ne "0"  ]; then
	echo 'solaris.smf.manage.mysql:::aa::' >>/etc/security/auth_attr
else
        echo "mysql smf auth already done"

fi

svccfg -s mysql setprop general/action_authorization=astring: 'solaris.smf.manage.mysql'
svccfg -s mysql setprop general/value_authorization=astring:  'solaris.smf.manage.mysql'
svcadm refresh mysql
  
exit 0
