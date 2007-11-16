/*
 * StartAction.java
 *
 * Created on Oct 7, 2007, 7:48:06 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opensolaris.webstack.settings.tray.actions;

import java.awt.MenuItem;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import org.opensolaris.webstack.settings.execution.ServerStatus;
import org.opensolaris.webstack.settings.execution.ServersManager;
import org.opensolaris.webstack.settings.tray.Main;
import org.opensolaris.webstack.settings.tray.Tray;

/**
 *
 * @author root
 */
public class StartAction extends MenuItem {

    public StartAction(final Tray tray) {
        super(" " + Tray.getBundle().getString("LABEL_Start"));
        addActionListener(new ActionListener() {

                    public void actionPerformed(ActionEvent e) {
                        ServersManager.StartServers();
                        tray.updateIcon();
                    }
                });
    }

    @Override
    public boolean isEnabled() {
        final ServerStatus running = ServersManager.getRunningState();
        if (running.apacheRunning && running.mySqlRunning) {
            return false;

        } else if (!running.apacheRunning && !running.mySqlRunning) {
            return true;
        } else {
            return true;
        }
    }
}