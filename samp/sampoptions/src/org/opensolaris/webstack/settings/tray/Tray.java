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
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ImageIcon;
import javax.swing.UIManager;
import org.opensolaris.webstack.settings.execution.ProcessExecutor;
import org.opensolaris.webstack.settings.execution.ServerStatus;
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
    private StartAction startAction;
    private StopAction stopAction;
    //olf the current icon color for server status
    private String iconColor = "";

    public void setIcon(String name) {
        iconColor = name;
        URL url = this.getClass().getResource("resources/server.png");

        Image image1 = new ImageIcon(url).getImage();
        Image badge = new ImageIcon(this.getClass().getResource("resources/" + name + ".png")).getImage();
        Image merged = Util.mergeImages(image1, badge, 16, 16);
        trayIcon.setImage(merged);
        if (ui != null) {
        //   ui.getJTabbedPane().setIconAt(0, new ImageIcon(badge));
        }

    }

    public void updateIcon() {
        java.awt.EventQueue.invokeLater(new Runnable() {

            @Override
            public void run() {
                final ServerStatus running = ServersManager.getRunningState();
                if (running.apacheRunning && running.mySqlRunning && !iconColor.equals("green")) {
                    setIcon("green");
                    startAction.setEnabled(false);
                    stopAction.setEnabled(true);

                } else if (!running.apacheRunning && !running.mySqlRunning && !iconColor.equals("red")) {
                    setIcon("red");
                    startAction.setEnabled(true);
                    stopAction.setEnabled(false);
                } else if (!iconColor.equals("yellow")) {
                    setIcon("yellow");
                    startAction.setEnabled(true);
                    stopAction.setEnabled(true);

                }

            }
        });
    }

    public Tray(HttpdConfModel model) {
        this.model = model;

        final PopupMenu popup = new PopupMenu();
        startAction = new StartAction(this);
        stopAction = new StopAction(this);
        if (SystemTray.isSupported()) {

            final SystemTray tray = SystemTray.getSystemTray();

            MouseListener mouseListener = new MouseListener() {

                public void mouseClicked(MouseEvent e) {
                    //  System.out.println
                    //( "Click at (" + e.getX() + ":" + e.getY() + ")" );
                    if (e.getClickCount() == 2) {
                        Main.showOptions();
                    }

                }

                public void mouseEntered(MouseEvent e) {
                //  System.out.println("Tray Icon - Mouse entered!");
                }

                @Override
                public void mouseExited(MouseEvent e) {
                // System.out.println("Tray Icon - Mouse exited!");
                }

                @Override
                public void mousePressed(MouseEvent e) {
                //System.out.println("Tray Icon - Mouse pressed!");
                }

                @Override
                public void mouseReleased(MouseEvent e) {
                    //System.out.println("Tray Icon - Mouse released!");
                    updateIcon();
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
                    Main.showOptions();
                }
            });

            //            popup.add(defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Administer_MySQL")));
//            defaultItem.setFont(defaultFont);
//            defaultItem.addActionListener(new ActionListener() {
//
//                @Override
//                public void actionPerformed(ActionEvent e) {
//                    System.out.println("admin mysql");
//                }
//            });
            Menu logsMenu = new Menu(" " + getBundle().getString("LABEL_Logs"));
            logsMenu.setFont(defaultFont);
            popup.add(logsMenu);

            logsMenu.add(defaultItem = new MenuItem("Apache log"));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    String cmd[] = {
                        "gnome-terminal",
                        "--hide-menubar",
                        "--title",
                        "Apache2 Log File (CTRL-C to finish)",
                        "--execute",
                        "tail",
                        "-20000f",
                        Environment.getApachelog()
                    };

                    ProcessExecutor.executeCommand(cmd);


                }
            });


            logsMenu.add(defaultItem = new MenuItem("MySql log"));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    String cmd[] = {
                        "gnome-terminal",
                        "--hide-menubar",
                        "--title",
                        "MySQL Log File (CTRL-C to finish)",
                        "--execute",
                        "tail",
                        "-20000f",
                        Environment.getMysqllog()
                    };
                    ProcessExecutor.executeCommand(cmd);
                }
            });


            popup.add(defaultItem = new MenuItem(" View Getting Started Guide"));
            defaultItem.setFont(defaultFont);
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    try {
                        URI uri = new URI("file:///opt/webstack/doc/html/index.html");
                        Desktop.getDesktop().browse(uri);
                    } catch (Exception ex) {
                        Logger.getLogger(Tray.class.getName()).log(Level.SEVERE, null, ex);
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
            popup.add(defaultItem = new MenuItem("Web Stack"));
            defaultItem.setFont(new Font("Monospaced", Font.BOLD, 24));
            defaultItem.setEnabled(false);
            URL url = this.getClass().getResource("resources/server.png");

            Image image1 = new ImageIcon(url).getImage();
            trayIcon = new TrayIcon(image1, getBundle().getString("TOOLTIP_Samp_Tooling"), popup);
            updateIcon();
            //            ActionListener actionListener = new ActionListener() {
//
//                        public void actionPerformed(ActionEvent e) {
//                            trayIcon.displayMessage("Solaris Web Stack Tray", "Use the right mouse button to display the tray menu.", TrayIcon.MessageType.INFO);
//                        }
//                    };




            trayIcon.setImageAutoSize(true);
            //   trayIcon.addActionListener(actionListener);
            trayIcon.addMouseListener(mouseListener);

            javax.swing.SwingUtilities.invokeLater(new Runnable() {

                @Override
                public void run() {
                    try {
                        URL url = this.getClass().getResource("resources/server.png");

                        Image image1 = new ImageIcon(url).getImage();
                        TrayIcon dummy = new TrayIcon(image1, "", null);
                        tray.add(dummy);
                        tray.add(trayIcon);
                        tray.remove(dummy);

                    } catch (Exception ex) {
                        Logger.getLogger(Tray.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            });


        } else {
            System.err.println("System tray is currently not supported.");
        }
    }

    public static java.util.ResourceBundle getBundle() {
        return java.util.ResourceBundle.getBundle("org/opensolaris/webstack/settings/tray/Bundle");
    }
}
