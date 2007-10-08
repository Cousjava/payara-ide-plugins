/*
 * StartAction.java
 *
 * Created on Oct 7, 2007, 7:48:06 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.tray.actions;

import java.awt.MenuItem;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import samp.execution.ServersManager;
import samp.model.Environment;
import samp.tray.Tray;

/**
 *
 * @author root
 */
public class StartAction extends MenuItem {

    public StartAction(final Tray tray) {
        super(" " + tray.getBundle().getString("LABEL_Start"));
        addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                ServersManager.StartServers();
                tray.setIcon("green");
            }
        });
    }

    public boolean isEnabled() {
        return !ServersManager.isApacheRunning(Integer.parseInt(Environment.getApachePortNumber()), 1000);
    }
}