#! /bin/sh

ROOT_PATH=""
USERNAME=$1

echo "adding apache22 and mysql privileges to user ${USERNAME}"
usermod -P 'Apache 22 Administration','MySql 5 Administration' ${USERNAME}

exit 0
