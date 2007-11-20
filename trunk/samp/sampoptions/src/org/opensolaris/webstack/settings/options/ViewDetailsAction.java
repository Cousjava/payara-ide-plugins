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
package org.opensolaris.webstack.settings.options;

import java.awt.Desktop;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opensolaris.webstack.settings.execution.ProcessExecutor;
import org.opensolaris.webstack.settings.model.Environment;
import org.opensolaris.webstack.settings.model.Model;

/**
 *
 * @author ludo
 */
public class ViewDetailsAction {

    public static void performAction() {
        try {
            File tempFile = File.createTempFile("webstack", "details");
            tempFile.deleteOnExit();
            FileWriter fout = null;

            try {
                fout = new FileWriter(tempFile, false);

                fout.write("Apache 2.2 :" + "\n");
                fout.write("Location: " +Environment.getHttpdconf()+ "\n");
                fout.write("Port: " +Environment.getApachePortNumber()+ "\n");
                fout.write("Log: " +Environment.getApachelog()+ "\n");
                fout.write("MySQL 5.0 :" + "\n");
                fout.write("Location: " +Environment.getMySqlCnf()+ "\n");
                fout.write("Log: " +Environment.getMysqllog()+ "\n");
                fout.write("PHP 5.2.4 :" + "\n");
                fout.write("Location: " +Environment.getPhpini()+ "\n");
                fout.write("Log: " +Environment.getPhplog()+ "\n");
                String cmd[]={"/usr/bin/svcs", "-v","apache22", "mysql"};
                ArrayList<String> a =ProcessExecutor.executeCommand(cmd );
                fout.write("\nCommand: /usr/bin/svcs -v apache22 mysql" + "\n");
                for (int i =0;i<a.size();i++){
                    fout.write(a.get(i)+ "\n");                    
                }
                String cmd2[]={"/usr/bin/svcs", "-x","apache22", "mysql"};
                a =ProcessExecutor.executeCommand(cmd2 );
                fout.write("\nCommand: /usr/bin/svcs -x apache22 mysql" + "\n");
                for (int i =0;i<a.size();i++){
                    fout.write(a.get(i)+ "\n");                    
                }
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                try {
                    fout.close();
                } catch (IOException ex) {
                    Logger.getLogger(Model.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            Desktop desktop = null;
            // Before more Desktop API is used, first check
                    // whether the API is supported by this particular
                    // virtual machine (VM) on this particular host.
            if (Desktop.isDesktopSupported()) {
                desktop = Desktop.getDesktop();
            }
            if (desktop.isSupported(Desktop.Action.OPEN)) {
                try {

                    desktop.open(tempFile);
                } catch (IOException ex) {
                    Logger.getLogger(ViewDetailsAction.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        } catch (IOException ex) {
            Logger.getLogger(ViewDetailsAction.class.getName()).log(Level.SEVERE, null, ex);
        }

    }
}
