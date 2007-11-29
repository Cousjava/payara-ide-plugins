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
package org.opensolaris.webstack.settings.tray;

import java.util.ArrayList;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import org.opensolaris.webstack.settings.execution.ProcessExecutor;
import org.opensolaris.webstack.settings.model.HttpdConfModel;
import org.opensolaris.webstack.settings.options.OptionsContainer;

/**
 *
 * @author ludo
 */
public class Main {

    private static HttpdConfModel model = new HttpdConfModel();
    private static OptionsContainer ui = null;

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        if (System.getProperty("os.name").startsWith("Mac") == false) {
            installGTK();
        }
        testSingleton();//may exit

        Tray tr = new Tray(model);
        new StatusThread(tr);
        if (args.length == 1) {
            if (args[0].equals("options")) {
                showOptions();
            }
        }
    }

    private static void installGTK() {
        try {
            String GTK = "com.sun.java.swing.plaf.gtk.GTKLookAndFeel";
            UIManager.setLookAndFeel(GTK);
            UIManager.installLookAndFeel("GTK", GTK);
        } catch (Exception e) {
            System.err.println("Could not install GTK");
        }
    }

    public static HttpdConfModel getHttpdConfModel() {
        return model;
    }

    public static void showOptions() {
        if (ui == null) {
            ui = new OptionsContainer(model);
        }
        java.awt.EventQueue.invokeLater(new Runnable() {

            @Override
            public void run() {

                ui.setVisible(false);
            }
        });
        java.awt.EventQueue.invokeLater(new Runnable() {

            public void run() {

                ui.setVisible(true);
                ui.toFront();
            }
        });
    }

    private static void testSingleton() {
        try {
            String cmd[] = {"/usr/java/bin/jps", "-l"};

            ArrayList<String> ret = ProcessExecutor.executeCommand(cmd);
            int count=0;
            for (int i = 0; i < ret.size(); i++) {
                if (ret.get(i).endsWith("/opt/webstack/bin/sampoptions.jar")) {
                    count++;
                }
                if (count>1){//2 means that this one instance and another one is up and running!
                    JOptionPane.showMessageDialog(null, "Web Stack Option already running. Check the desktop tray area.");
                    System.exit(0);
                }

            }
        }catch  (Exception e) {
        //do nothing if for some reason we cannot detect if it is running or not
        }
    }
}
