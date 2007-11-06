#! /bin/sh

/usr/sbin/svcadm -v enable svc:/network/http:apache2
/usr/sbin/svcadm -v enable svc:/application/database/mysql:version_50

sleep 3
/usr/bin/firefox http://localhost &

