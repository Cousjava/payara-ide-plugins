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
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ludo
 */
public class ServersManager {

    static public void StartServers() {
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
                URI uri = new URI("http://localhost:8888");
                desktop.browse(uri);
            } catch (IOException ex) {
                Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
            } catch (URISyntaxException ex2) {
                Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex2);
            }
        }
    }
        static public void StopServers() {
            System.out.println("stopping apache2 and MySql");
        }

}