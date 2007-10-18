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
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opensolaris.webstack.settings.model.Environment;

/**
 *
 * @author ludo
 */
public class ServersManager {

    static public void StartServers() {
        try {
            // svcadm enable apache2
            String s[]=Environment.getStartapache().split("[ ]");
            Process p1;
            if (s.length==1){
                p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
                p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }
            consumeIOs(p1,System.out);
            s=Environment.getStartmysql().split("[ ]");
            if (s.length==1){
                 p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
                p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }
            consumeIOs(p1,System.out);
 
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
                         
                    URI uri = new URI("http://localhost:"+Environment.getApachePortNumber());
                    desktop.browse(uri);
                } catch (IOException ex) {
                    Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
                } catch (URISyntaxException ex2) {
                    Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex2);
                }
            }
        } catch (IOException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, ex.getMessage(), "");
        }
    }
        static public void StopServers() {
        try {
//            System.out.println("stopping apache2 and MySql");
            // svcadm disable apache2
            String s[]=Environment.getStopapache().split("[ ]");
            Process p1;
            if (s.length==1){
                 p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
                p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }
                        consumeIOs(p1,System.out);

            s=Environment.getStopmysql().split("[ ]");
            if (s.length==1){
                 p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
                p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }
            consumeIOs(p1,System.out);
        } catch (IOException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, ex.getMessage(), "");
        }
        }

        
        static private void consumeIOs(Process child , OutputStream outs) {
            //
            // Attach to the process's stdout, and ignore what comes back.
            //
            final Thread[] copyMakers = new Thread[2];
            OutputStreamWriter oss = null;
            if (outs != null) {
                oss=new OutputStreamWriter(outs);
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
            /** Return true if a Apache server is running on the specifed port */
    public static boolean isApacheRunning(int port, int timeout) {
        Socket socket = new Socket();
        try {
            try {
                socket.connect(new InetSocketAddress("localhost", port), timeout); // NOI18N
                socket.setSoTimeout(timeout);
                PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
                try {
                    BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                    try {
                        // request
                        out.println("HEAD /webstack-apache-test-if-running HTTP/1.1\nHost: localhost:" + port + "\n"); // NOI18N

                        // response
                        String text = in.readLine();
                        System.out.println("text="+text);
                        if (text == null || !text.startsWith("HTTP/")) { // NOI18N
                            return false; // not an http response
                        }
                        Map headerFileds = new HashMap();
                        while ((text = in.readLine()) != null && text.length() > 0) {
                            int colon = text.indexOf(':');
                            if (colon <= 0) {
                                return false; // not an http header
                            }
                            String name = text.substring(0, colon).trim();
                            String value = text.substring(colon + 1).trim();
                            System.out.println("name"+name);
                            System.out.println("value"+value);
                            if (name.equals("Server"))

                            if (value.contains("Apache/2.")) { // NOI18N
                                    return true;
                                
                            } 
                        }
                        return false;
                    } finally {
                        in.close();
                    }
                } finally {
                    out.close();
                }
            } finally {
                socket.close();
            }
        } catch (IOException ioe) {
            return false;
        }
    }
}
