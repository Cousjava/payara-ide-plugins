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
import org.opensolaris.webstack.settings.execution.ServersManager;
import org.opensolaris.webstack.settings.tray.Main;
import org.opensolaris.webstack.settings.tray.Tray;


/**
 *
 * @author root
 */
public class StopAction extends MenuItem {

    public StopAction(final Tray tray) {
        super(" " + Tray.getBundle().getString("LABEL_Stop"));
        addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                ServersManager.StopServers();
                tray.updateIcon();
            }
        });
    }

    @Override
    public boolean isEnabled() {
        return ServersManager.isApacheRunning(Main.getHttpdConfModel().getPortNumber(), 1000);
    }
}