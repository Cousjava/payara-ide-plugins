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
*/
/*
 * Copyright 2007 Sun Microsystems, Inc.  All rights reserved.
* Use is subject to license terms.
*/
package org.opensolaris.webstack.settings.execution;

import java.awt.Desktop;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opensolaris.webstack.settings.model.Environment;

/**
 *
 * @author ludo
 */
public class ServersManager {
    /*
 * Exec=gnome-terminal   'Apache2 Log File (CTRL-C to finish)'  tail   -20000f /var/apache2/2.2/logs/error_log

 */

    static private String C1 = "/usr/bin/gnome-terminal";//NOI18N

    static private String C2 = "--hide-menubar";//NOI18N

    static private String C3 = "--title";//NOI18N

    static private String C4 = "--execute";//NOI18N

    static public void StartServers() {

        URL url = ServersManager.class.getProtectionDomain().getCodeSource().getLocation();
        String dir = url.getFile();
        //  System.out.println(" dir=" + dir);
        try {
            // svcadm enable apache2
            String s[] = {C1, C2, C3, "'WebStack'", C4, "/opt/webstack/bin/start.sh"};
            Process p1;

            p1 = new ProcessBuilder(s).start();

            consumeIOs(p1, System.out);
            Thread.sleep(1000);
            displayHomePage();


        } catch (InterruptedException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, ex.getMessage(), "");
        }
    }

    static public void displayHomePage() {
        Desktop desktop = null;
        // Before more Desktop API is used, first check
            // whether the API is supported by this particular
            // virtual machine (VM) on this particular host.
        if (Desktop.isDesktopSupported()) {
            desktop = Desktop.getDesktop();
        }
        if (desktop.isSupported(Desktop.Action.BROWSE)) {
            try {
                // launch browser

                URI uri = new URI("http://localhost:" + Environment.getApachePortNumber());
                desktop.browse(uri);
            } catch (IOException ex) {
                Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
            } catch (URISyntaxException ex2) {
                Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex2);
            }
        }
    }

    static public void StopServers() {
        try {
            //            System.out.println("stopping apache2 and MySql");
            // svcadm disable apache2
            String s[] = {C1, C2, C3, "'WebStack'", C4, "/opt/webstack/bin/stop.sh"};
            Process p1;

            p1 = new ProcessBuilder(s).start();
            consumeIOs(p1, System.out);
            Thread.sleep(1000);
        } catch (InterruptedException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, ex.getMessage(), "");
        }
    }

    static public void RestartServers() {
        try {
            //            System.out.println("stopping apache2 and MySql");
            // svcadm disable apache2
            String s[] = {C1, C2, C3, "'WebStack'", C4, "/opt/webstack/bin/restart.sh"};
            Process p1;

            p1 = new ProcessBuilder(s).start();
            consumeIOs(p1, System.out);
            Thread.sleep(1000);
            displayHomePage();
        } catch (InterruptedException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, ex.getMessage(), "");
        }
    }

    static private void consumeIOs(Process child, OutputStream outs) {
        //
            // Attach to the process's stdout, and ignore what comes back.
            //
        final Thread[] copyMakers = new Thread[2];
        OutputStreamWriter oss = null;
        if (outs != null) {
            oss = new OutputStreamWriter(outs);
        }
        (copyMakers[0] = new ProcessExecutor.OutputCopier(new InputStreamReader(child.getInputStream()), oss, true)).start();
        (copyMakers[1] = new ProcessExecutor.OutputCopier(new InputStreamReader(child.getErrorStream()), oss, true)).start();
        try {
            //int ret =
            child.waitFor();
            Thread.sleep(1000);  // time for copymakers
        } catch (InterruptedException e) {
        } finally {
            try {
                copyMakers[0].interrupt();
                copyMakers[1].interrupt();
            } catch (Exception e) {
            }
        }
    }


    public static ServerStatus getRunningState() {
        String[] s = {"/usr/bin/svcs", "apache22", "mysql"};
        Process process;
        ServerStatus ret = new ServerStatus();
        try {
            process = new ProcessBuilder(s).start();
            InputStream is = process.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line;

            while ((line = br.readLine()) != null) {
                if (line.indexOf("mysql") >= 0) {
                    ret.mySqlRunning = (line.indexOf("online") >= 0);
                } else if (line.indexOf("apache22") >= 0) {
                    ret.apacheRunning = (line.indexOf("online") >= 0);
                }
            }
        } catch (IOException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        return ret;
    }
}
