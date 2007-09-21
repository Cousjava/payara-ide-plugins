#! /bin/sh

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
   *) shift;;
   esac
done

#path to script
SCRIPT=${LOC}/main.sh

if [ -x /sbin/center_window ] ; then
   # Use a special window available only in the Solaris miniroot
   /usr/dt/bin/dtterm -geometry 80x32+300+300 \
	-name DeveloperTools \
	-e /sbin/sh -c "/sbin/center_window SAMPConfiguration;${SCRIPT} ${ROOT_OPTION}"
else
   /usr/dt/bin/dtterm -geometry 80x32+300+300 -e /sbin/sh ${SCRIPT} ${ROOT_OPTION}
fi

exit 0
