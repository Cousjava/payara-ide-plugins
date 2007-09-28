/*
 * ServersManager.java
 *
 * Created on Sep 21, 2007, 2:29:49 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.execution;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;
import samp.model.Environment;
import samp.model.Util;

/**
 *
 * @author ludo
 */
public class ServersManager {

    static public void StartServers() {
        try {
            // svcadm enable apache2
            String s[]=Environment.getStartapache().split("[ ]");
            if (s.length==1){
                Process p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
               Process p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }
            s=Environment.getStartmysql().split("[ ]");
            if (s.length==1){
                Process p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
               Process p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }
 
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
                    String port=Util.getValue(new File(Environment.getHttpdconf()), "Listen ");
                    System.out.println("port is"+port);
                          
                    URI uri = new URI("http://localhost:"+port);
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
            System.out.println("stopping apache2 and MySql");
            // svcadm disable apache2
            String s[]=Environment.getStopapache().split("[ ]");
            if (s.length==1){
                Process p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
               Process p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }
            s=Environment.getStopmysql().split("[ ]");
            if (s.length==1){
                Process p1 = new ProcessBuilder(s[0],"").start();
            }
            else{
               Process p1 = new ProcessBuilder(s[0],s[1],s[2]).start();
                 
            }        } catch (IOException ex) {
            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, ex.getMessage(), "");
        }
        }

}