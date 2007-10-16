#! /bin/sh

/usr/sbin/svcadm -v enable svc:/network/http:apache2
#svcadm enable svc:/network/cswmysql5:default
sleep 3
