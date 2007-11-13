#! /bin/sh

ROOT_PATH=""
USERNAME=$1

echo "adding apache22 and mysql privileges to user ${USERNAME}"
usermod -P 'Apache 22 Administration','MySql 5 Administration' ${USERNAME}


setfacl -m user:${USERNAME}:rw- /etc/apache2/2.2/httpd.conf
setfacl -m mask:rw- /etc/apache2/2.2/httpd.conf     
setfacl -m user:${USERNAME}:rw- /etc/php5/5.2.4/php.ini 
setfacl -m mask:rw- /etc/php5/5.2.4/php.ini   
setfacl -m user:${USERNAME}:rw- /etc/mysql/5.0/my.cnf 
setfacl -m mask:rw- /etc/mysql/5.0/my.cnf   
exit 0
