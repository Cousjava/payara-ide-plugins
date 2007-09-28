/*
 * Environment.java
 *
 * Created on Sep 28, 2007, 9:08:37 AM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.model;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

/**
 *
 * @author ludo
 */
public class Environment {

    private static String httpdconf = "/etc/apache2/httpd.conf";
    private static String phpini = "/etc/php5/php.ini";
    private static String apachelog = "/var/apache2/logs/error_log";
    private static String mysqllog = "/var/logs/mysql_error_log";
    private static String phplog = "/var/php5/logs/php_error.log";
    private static String startapache = "svcadm enable apache2";
    private static String stopapache = "svcadm disable apache2";
    private static String startmysql = "svcadm enable svc:/network/cswmysql5:default";
    private static String stopmysql = "svcadm disable svc:/network/cswmysql5:default";

static{
        // Read properties file.
        Properties properties = new Properties();
        try {
            properties.load(new FileInputStream(System.getProperty("user.home") + ".sampoptions.properties"));
            httpdconf= properties.getProperty("httpdconf");
            phpini= properties.getProperty("phpini");
            apachelog= properties.getProperty("apachelog");
            mysqllog= properties.getProperty("mysqllog");
            phplog= properties.getProperty("phplog");
            startapache= properties.getProperty("startapache");
            stopapache= properties.getProperty("stopapache");
            startmysql= properties.getProperty("startmysql");
            stopmysql= properties.getProperty("stopmysql");
        } catch (IOException e) {
        }
        
    }

    public static String getHttpdconf() {
        return httpdconf;
    }

    public static String getPhpini() {
        return phpini;
    }

    public static String getApachelog() {
        return apachelog;
    }

    public static String getMysqllog() {
        return mysqllog;
    }

    public static String getPhplog() {
        return phplog;
    }

    public static String getStartapache() {
        return startapache;
    }

    public static String getStopapache() {
        return stopapache;
    }

    public static String getStartmysql() {
        return startmysql;
    }

    public static String getStopmysql() {
        return stopmysql;
    }
    public static String getApachePortNumber(){
        return "80";
    }
}