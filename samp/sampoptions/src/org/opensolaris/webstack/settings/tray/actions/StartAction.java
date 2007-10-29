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
import org.opensolaris.webstack.settings.model.Environment;
import org.opensolaris.webstack.settings.tray.Main;
import org.opensolaris.webstack.settings.tray.Tray;

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
                tray.updateIcon();
            }
        });
    }

    @Override
    public boolean isEnabled() {
        System.out.println("isenalbe for action is called");
        return !ServersManager.isApacheRunning(Main.getHttpdConfModel().getPortNumber(), 1000);
    }
}