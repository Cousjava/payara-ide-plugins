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

import java.awt.Desktop;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;
import org.opensolaris.webstack.settings.tray.Main;

/**
 *
 * @author ludo
 */
public class Environment {

    private static String httpdconf = "/etc/apache2/2.2/httpd.conf";
    private static String phpini = "/etc/php5/5.2.4/php.ini";
    private static String mysqlcnf = "/etc/mysql/5.0/my.cnf";
    private static String ftpconf = "/etc/ftpd/ftpaccess";
    private static String apachelog = "/var/apache2/2.2/logs/error_log";
    private static String mysqllog = "/var/svc/log/application-database-mysql:version_50.log";
    private static String phplog = "/var/php5/logs/php_error.log";
    private static String port = null;

    static {
        // Read properties file.
        Properties properties = new Properties();
        File f = new File(System.getProperty("user.home") + "/.webstackoptions.properties");
        if (f.exists()) {
            FileInputStream fis = null;
            try {
                properties.load(fis = new FileInputStream(f));
                httpdconf = properties.getProperty("httpdconf");
                phpini = properties.getProperty("phpini");
                mysqlcnf = properties.getProperty("mysqlcnf");
                ftpconf = properties.getProperty("ftpconf");
                apachelog = properties.getProperty("apachelog");
                mysqllog = properties.getProperty("mysqllog");
                phplog = properties.getProperty("phplog");

            } catch (IOException e) {
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (Exception e) {
                    }
                }
            }
        } else {//create the file with default values
            FileOutputStream fos = null;
            try {
                properties.setProperty("httpdconf", httpdconf);
                properties.setProperty("phpini", phpini);
                properties.setProperty("mysqlcnf", mysqlcnf);
                properties.setProperty("ftpconf", ftpconf);
                properties.setProperty("apachelog", apachelog);
                properties.setProperty("mysqllog", mysqllog);
                properties.setProperty("phplog", phplog);

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

        //now test the environment to detect config errors:
        File testF;
        String errorMessage = "";
        testF = new File(getHttpdconf());
        if (!testF.exists()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " does not exist.\n";
        } else if (!testF.canWrite()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " is not writable.\n";
        }


        testF = new File(getPhpini());
        if (!testF.exists()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " does not exist.\n";
        } else if (!testF.canWrite()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " is not writable.\n";
        }
        testF = new File(getMySqlCnf());
        if (!testF.exists()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " does not exist.\n";
        } else if (!testF.canWrite()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " is not writable.\n";
        }
        testF = new File(getApachelog());
        if (!testF.exists()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " does not exist.\n";
        }
        testF = new File(getMysqllog());
        if (!testF.exists()) {
            errorMessage += "File: " + testF.getAbsolutePath() + " does not exist.\n";
        }
        if (!errorMessage.equals("")) {


            Object[] options = {"Edit Options Config ini file", "Close"};
            int n = JOptionPane.showOptionDialog(null, errorMessage, "Configuration Error", JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[1]);
            if (n == JOptionPane.OK_OPTION) {
                Desktop desktop = null;
                if (Desktop.isDesktopSupported()) {
                    desktop = Desktop.getDesktop();
                }
                if (desktop.isSupported(Desktop.Action.OPEN)) {
                    try {

                        desktop.open(f);
                    } catch (IOException ex) {
                        Logger.getLogger(Environment.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            }

            System.exit(0);
        }


    }

    public static String getHttpdconf() {
        File f = new File(httpdconf);
        if (f.exists()) {
            return httpdconf;
        } else { //temp solution for new and old apache stuff
            httpdconf = "/etc/apache2/httpd.conf";
            apachelog = "/var/apache2/logs/error_log";
            return httpdconf;
        }
    }

    public static String getPhpini() {
        return phpini;
    }

    public static String getMySqlCnf() {
        return mysqlcnf;
    }

    public static String getFTPConf() {
        return ftpconf;
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



    public static String getApachePortNumber() {

        return "" + Main.getHttpdConfModel().getPortNumber();
    }
}
