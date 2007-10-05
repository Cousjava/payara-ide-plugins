/*
 * Main.java
 *
 * Created on Sep 20, 2007, 3:54:46 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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

        installGTK();
        InstallMenus.initMenus();
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