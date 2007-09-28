package samp.tray;

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
import samp.execution.ServersManager;
import samp.model.Environment;
import samp.model.Util;
import samp.options.OptionsContainer;

public class Tray {

    public Tray() {

        final TrayIcon trayIcon;
        final PopupMenu popup = new PopupMenu();

        if (SystemTray.isSupported()) {

            final SystemTray tray = SystemTray.getSystemTray();
            URL url = this.getClass().getResource("resources/gnome-html.png");

            Image image1 = new ImageIcon(url).getImage();
            Image badge = new ImageIcon(this.getClass().getResource("resources/red.png")).getImage();
            Image merged = Util.mergeImages(image1, badge,26,26);
            MouseListener mouseListener = new MouseListener() {

                public void mouseClicked(MouseEvent e) {
                    System.out.println("Tray Icon - Mouse clicked!");
                }

                public void mouseEntered(MouseEvent e) {
                    System.out.println("Tray Icon - Mouse entered!");
                }

                public void mouseExited(MouseEvent e) {
                    System.out.println("Tray Icon - Mouse exited!");
                }

                public void mousePressed(MouseEvent e) {
                    System.out.println("Tray Icon - Mouse pressed!");
                }

                public void mouseReleased(MouseEvent e) {
                    System.out.println("Tray Icon - Mouse released!");
                }
            };




            MenuItem defaultItem;
            popup.add(defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Start")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("Start");
                    ServersManager.StartServers();
                }
            });
            popup.add(defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Stop")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("stop");
                    ServersManager.StopServers();
                }
            });
            popup.add(defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Options")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("options");

                    java.awt.EventQueue.invokeLater(new Runnable() {

                        public void run() {

                            OptionsContainer.getInstance().setVisible(true);
                            OptionsContainer.getInstance().toFront();
                        }
                    });
                }
            });

            popup.add(defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Administer_MySQL")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("admin mysql");
                }
            });
            Menu logsMenu = new Menu(" " + getBundle().getString("LABEL_Logs"));
            popup.add(logsMenu);
            logsMenu.add(defaultItem = new MenuItem("php log"));
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
            logsMenu.add(defaultItem = new MenuItem("apache log"));
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
            logsMenu.add(defaultItem = new MenuItem("MySql log"));
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

                            desktop.open(new File(Environment.getMysqllog()));
                        } catch (IOException ex) {
                            Logger.getLogger(ServersManager.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                }
            });            
            defaultItem = new MenuItem(" " + getBundle().getString("LABEL_Exit"));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("Exiting");
                    System.exit(0);
                }
            });
            popup.add(defaultItem);

            //empty one
            popup.add(defaultItem = new MenuItem("SAMP Console"));
            defaultItem.setFont(new Font("Monospaced", Font.BOLD, 24));
            defaultItem.setEnabled(false);
            trayIcon = new TrayIcon(merged, getBundle().getString("TOOLTIP_Samp_Tooling"), popup);


            ActionListener actionListener = new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    trayIcon.displayMessage("Action Event", "An Action Event Has Been Peformed!", TrayIcon.MessageType.INFO);
                    trayIcon.displayMessage("Action Exdvfxdfvent", "An fdfdfdfdfAction Event Has Been Peformed!", TrayIcon.MessageType.INFO);
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
        return java.util.ResourceBundle.getBundle("samp/tray/Bundle");
    }
}