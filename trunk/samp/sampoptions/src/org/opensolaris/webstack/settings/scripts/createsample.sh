#! /bin/sh
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
