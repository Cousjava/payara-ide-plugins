/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License, Version 1.0 only
 * (the "License").  You may not use this file except in compliance
 * with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 *//*
 * Copyright 2007 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */
package org.opensolaris.webstack.settings.model;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

/**
 *
 * @author ludo
 */
public class Environment {

    private static String httpdconf = "/etc/apache2/httpd.conf";
    private static String phpini = "/etc/php5/5.2.4/php.ini";
    private static String apachelog = "/var/apache2/logs/error_log";
    private static String mysqllog = "/opt/csw/mysql5/var/solaris-devx.err";
    private static String phplog = "/var/php5/logs/php_error.log";
    private static String startapache = "svcadm enable apache2";
    private static String stopapache = "svcadm disable apache2";
    private static String startmysql = "svcadm enable svc:/network/cswmysql5:default";
    private static String stopmysql = "svcadm disable svc:/network/cswmysql5:default";
    private static String port = null;

    static {
        // Read properties file.
        Properties properties = new Properties();
        File f = new File(System.getProperty("user.home") + "/.sampoptions.properties");
        if (f.exists()){
        FileInputStream fis=null;
        try {
            properties.load(fis= new FileInputStream(f));
            httpdconf = properties.getProperty("httpdconf");
            phpini = properties.getProperty("phpini");
            apachelog = properties.getProperty("apachelog");
            mysqllog = properties.getProperty("mysqllog");
            phplog = properties.getProperty("phplog");
            startapache = properties.getProperty("startapache");
            stopapache = properties.getProperty("stopapache");
            startmysql = properties.getProperty("startmysql");
            stopmysql = properties.getProperty("stopmysql");
        } catch (IOException e) {
        } finally{
            if (fis!=null){
                try{
                fis.close();
                } catch (Exception e){}
            }
        }
        } else {//create the file with default values
            FileOutputStream fos = null;
            try {
                properties.setProperty("httpdconf", httpdconf);
                properties.setProperty("phpini", phpini);
                properties.setProperty("apachelog", apachelog);
                properties.setProperty("mysqllog", mysqllog);
                properties.setProperty("phplog", phplog);
                properties.setProperty("startapache", startapache);
                properties.setProperty("stopapache", stopapache);
                properties.setProperty("startmysql", startmysql);
                properties.setProperty("stopmysql", stopmysql);
                fos = new FileOutputStream(f);
                properties.store(fos, "");
            } catch (IOException e) {
            } finally {
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (Exception e) {
                    }
                }
        }           
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

    public static String getApachePortNumber() {
        if (port == null) {
            port = Util.getValue(new File(getHttpdconf()), "Listen ");
        }
        return port;
    }
}