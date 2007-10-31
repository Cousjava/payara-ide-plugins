#! /bin/sh

/usr/sbin/svcadm -v disable svc:/network/http:apache2
/usr/sbin/svcadm -v disable svc:/application/database/mysql:version_50
sleep 3