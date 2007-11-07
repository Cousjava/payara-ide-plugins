#! /bin/sh
FILE="/var/apache2/2.2/htdocs/phpsample"
if test -d $FILE
then
       echo "PHP sample already installed at " $FILE
else
        unzip /opt/webstack/sample/phpsample.zip -d /var/apache2/2.2/htdocs
   
fi
exit 0
