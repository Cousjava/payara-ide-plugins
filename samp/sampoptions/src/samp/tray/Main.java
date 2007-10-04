/*
 * Main.java
 *
 * Created on Sep 20, 2007, 3:54:46 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.tray;

import java.io.File;
import javax.swing.UIManager;

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
        initMenus();
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
    
    private static void initMenus(){
        String homeDir = System.getProperty("user.home");
        System.out.println("home dir"+homeDir);
        File apps= new File(homeDir,".local/share/applications");

        if (apps.exists()==false){
            apps.mkdirs();           
        }
        else {
            return;            
        }
        File desktop= new File(homeDir,".local/share/desktop-directories");

        if (desktop.exists()==false){
            desktop.mkdirs();           
        }        
         File merged= new File(homeDir,".config/menus/applications-merged");

        if (merged.exists()==false){
            merged.mkdirs();           
        }
    }
}