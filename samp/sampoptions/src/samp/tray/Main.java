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

package samp.tray;

import javax.swing.UIManager;
import samp.scripts.startmenus.InstallMenus;

/**
 *
 * @author ludo
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        if (System.getProperty("os.name").startsWith("Mac") == false) {
            installGTK();
            InstallMenus.initMenus();
        }
        new Tray();
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
}