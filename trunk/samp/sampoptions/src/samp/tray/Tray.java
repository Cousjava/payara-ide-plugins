package samp.tray;

import java.awt.AWTException;
import java.awt.Image;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import javax.swing.ImageIcon;
import samp.options.OptionsContainer;

public class Tray {

    public Tray() {

        final TrayIcon trayIcon;

        if (SystemTray.isSupported()) {

            SystemTray tray = SystemTray.getSystemTray();
            URL url = this.getClass().getResource("resources/samp32.jpg");

            Image image = new ImageIcon(url).getImage();

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

            ActionListener exitListener = new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println(getBundle().getString("LABEL_Exiting"));
                    System.exit(0);
                }
            };

            PopupMenu popup = new PopupMenu();

            popup.add(new MenuItem(getBundle().getString("LABEL_Start")));
            popup.add(new MenuItem(getBundle().getString("LABEL_Stop")));
            popup.add(new MenuItem(getBundle().getString("LABEL_Options")));
            popup.add(new MenuItem(getBundle().getString("LABEL_Administer_MySQL")));
            popup.add(new MenuItem(getBundle().getString("LABEL_Logs")));

            MenuItem defaultItem = new MenuItem(getBundle().getString("LABEL_Exit"));
            defaultItem.addActionListener(exitListener);
            popup.add(defaultItem);
            trayIcon = new TrayIcon(image, getBundle().getString("TOOLTIP_Samp_Tooling"), popup);

            ActionListener actionListener = new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    trayIcon.displayMessage("Action Event", "An Action Event Has Been Peformed!", TrayIcon.MessageType.INFO);
                    java.awt.EventQueue.invokeLater(new Runnable() {

                        public void run() {
                            new OptionsContainer().setVisible(true);
                        }
                    });
                }
            };




            trayIcon.setImageAutoSize(true);
            trayIcon.addActionListener(actionListener);
            trayIcon.addMouseListener(mouseListener);

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