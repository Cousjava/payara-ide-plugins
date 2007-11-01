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
 *//*
 * Copyright 2007 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */
package org.opensolaris.webstack.settings.tray;

import java.awt.AWTException;
import java.awt.Desktop;
import java.awt.Font;
import java.awt.Image;
import java.awt.Menu;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ImageIcon;
import javax.swing.UIManager;
import org.opensolaris.webstack.settings.execution.ServersManager;
import org.opensolaris.webstack.settings.model.Environment;
import org.opensolaris.webstack.settings.model.HttpdConfModel;
import org.opensolaris.webstack.settings.model.Util;
import org.opensolaris.webstack.settings.options.OptionsContainer;
import org.opensolaris.webstack.settings.tray.actions.StartAction;
import org.opensolaris.webstack.settings.tray.actions.StopAction;


public class Tray {

    private TrayIcon trayIcon;
    private HttpdConfModel model;
    private OptionsContainer ui = null;
    private  StartAction startAction;
    private  StopAction stopAction;
    public void setIcon(String name) {
        URL url = this.getClass().getResource("resources/apache.png");

        Image image1 = new ImageIcon(url).getImage();
        Image badge = new ImageIcon(this.getClass().getResource("resources/" + name + ".png")).getImage();
        Image merged = Util.mergeImages(image1, badge, 18, 18);
        trayIcon.setImage(merged);
    }
    public void updateIcon(){
            boolean running = ServersManager.isApacheRunning(model.getPortNumber(), 1000);
            if (running) {
                setIcon("green");
            } else {
                setIcon("red");
            }
            startAction.setEnabled(!running);
            stopAction.setEnabled(running);
    }
    public void showOptions() {
        if (ui==null){
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

    public Tray(HttpdConfModel model) {
        this.model = model;
        
        final PopupMenu popup = new PopupMenu();
        startAction= new StartAction(this);
        stopAction= new StopAction(this);
        if (SystemTray.isSupported()) {

            final SystemTray tray = SystemTray.getSystemTray();

            MouseListener mouseListener = new MouseListener() {

                        public void mouseClicked(MouseEvent e) {
                            System.out.println("Tray Icon - Mouse clicked!");
                        }

                        public void mouseEntered(MouseEvent e) {
                            System.out.println("Tray Icon - Mouse entered!");
                        }

                @Override
                        public void mouseExited(MouseEvent e) {
                            System.out.println("Tray Icon - Mouse exited!");
                        }

                @Override
                        public void mousePressed(MouseEvent e) {
                            System.out.println("Tray Icon - Mouse pressed!");
                        }

                @Override
                        public void mouseReleased(MouseEvent e) {
                            System.out.println("Tray Icon - Mouse released!");
                        }
                    };




            Font defaultFont = (Font) UIManager.get("Label.font");
            MenuItem defaultItem;
            popup.add(startAction);
            startAction.setFont(defaultFont);

            popup.add(stopAction);
            stopAction.setFont(defaultFont);
            
            
            popup.add(defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Options")));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    showOptions();
                }
            });

            popup.add(defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Administer_MySQL")));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    System.out.println("admin mysql");
                }
            });
            Menu logsMenu = new Menu(" " + getBundle().getString("LABEL_Logs"));
            logsMenu.setFont(defaultFont);
            popup.add(logsMenu);

            logsMenu.add(defaultItem = new MenuItem("Apache log"));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("logs");
                    Desktop desktop = null;
                    // Before more Desktop API is used, first check
                    // whether the API is supported by this particular
                    // virtual machine (VM) on this particular host.
                    if (Desktop.isDesktopSupported()) {
                        desktop = Desktop.getDesktop();
                    }
                    if (desktop.isSupported(Desktop.Action.OPEN)) {
                        try {

                            desktop.open(new File(Environment.getApachelog()));
                        } catch (IOException ex) {
                            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                }
            });
                        logsMenu.add(defaultItem = new MenuItem("PHP log"));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("logs");
                    Desktop desktop = null;
                    // Before more Desktop API is used, first check
                    // whether the API is supported by this particular
                    // virtual machine (VM) on this particular host.
                    if (Desktop.isDesktopSupported()) {
                        desktop = Desktop.getDesktop();
                    }
                    if (desktop.isSupported(Desktop.Action.OPEN)) {
                        try {

                            desktop.open(new File(Environment.getPhplog()));
                        } catch (IOException ex) {
                            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                }
            });
            logsMenu.add(defaultItem = new MenuItem("MySql log"));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("MySql logs");
                    Desktop desktop = null;
                    // Before more Desktop API is used, first check
                    // whether the API is supported by this particular
                    // virtual machine (VM) on this particular host.
                    if (Desktop.isDesktopSupported()) {
                        desktop = Desktop.getDesktop();
                    }
                    if (desktop.isSupported(Desktop.Action.OPEN)) {
                        try {

                            desktop.open(new File(Environment.getMysqllog()));
                        } catch (IOException ex) {
                            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                }
            });
            defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Exit"));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.exit(0);
                }
            });
            popup.add(defaultItem);

            //empty one
            popup.add(defaultItem = new MenuItem("WebStack"));
            defaultItem.setFont(new Font("Monospaced", Font.BOLD, 24));
            defaultItem.setEnabled(false);
            URL url = this.getClass().getResource("resources/apache.png");

            Image image1 = new ImageIcon(url).getImage();
            trayIcon = new TrayIcon(image1, getBundle().getString("TOOLTIP_Samp_Tooling"), popup);
            updateIcon();
            ActionListener actionListener = new ActionListener() {

                        public void actionPerformed(ActionEvent e) {
                            trayIcon.displayMessage("Solaris WebStack Tray", "Use the right mouse button to display the tray menu.", TrayIcon.MessageType.INFO);
                        }
                    };




            trayIcon.setImageAutoSize(true);
            trayIcon.addActionListener(actionListener);
            // trayIcon.addMouseListener(mouseListener);
            try {
                tray.add(trayIcon);
            } catch (AWTException e) {
                System.err.println("TrayIcon could not be added.");
            }
        } else {
            System.err.println("System tray is currently not supported.");
        }
    }

    public static java.util.ResourceBundle getBundle() {
        return java.util.ResourceBundle.getBundle("org/opensolaris/webstack/settings/tray/Bundle");
    }
}