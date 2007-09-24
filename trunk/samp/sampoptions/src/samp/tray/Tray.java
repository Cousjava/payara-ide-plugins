package samp.tray;

import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JWindow;
import javax.swing.SwingUtilities;
import samp.execution.ServersManager;
import samp.options.OptionsContainer;

public class Tray {

    public Tray() {

        final TrayIcon trayIcon;
        final PopupMenu popup = new PopupMenu();
        

        if (SystemTray.isSupported()) {

            final SystemTray tray = SystemTray.getSystemTray();
            URL url = this.getClass().getResource("resources/gnome-html.png");

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
                    new aa();
                }

                public void mouseReleased(MouseEvent e) {
                    System.out.println("Tray Icon - Mouse released!");
                }
            };




            MenuItem defaultItem;
            popup.add(defaultItem = new MenuItem(" "+getBundle().getString("LABEL_Start")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("Start");
                    ServersManager.StartServers();
                }
            });
            popup.add(defaultItem = new MenuItem(" "+getBundle().getString("LABEL_Stop")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("stop");
                    ServersManager.StopServers();
                }
            });
            popup.add(defaultItem = new MenuItem(" "+getBundle().getString("LABEL_Options")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("options");
                    java.awt.EventQueue.invokeLater(new Runnable() {

                        public void run() {
                            new OptionsContainer().setVisible(true);
                        }
                    });
                }
            });
            
            popup.add(defaultItem = new MenuItem(" "+getBundle().getString("LABEL_Administer_MySQL")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("admin mysql");
                    new aa();
                }
            });
            popup.add(defaultItem = new MenuItem(" "+getBundle().getString("LABEL_Logs")));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("logs");
                }
            });
            defaultItem = new MenuItem(" "+getBundle().getString("LABEL_Exit"));
            defaultItem.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    System.out.println("Exiting");
                    System.exit(0);
                }
            });
            popup.add(defaultItem);
            
            //empty one
            popup.add(defaultItem = new MenuItem("SAMP Console"));
            defaultItem.setFont(new Font("Monospaced",Font.BOLD,24));
defaultItem.setEnabled(false);
            trayIcon = new TrayIcon(image, 
                    getBundle().getString("TOOLTIP_Samp_Tooling"), 
                    popup);


            ActionListener actionListener = new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    trayIcon.displayMessage("Action Event", "An Action Event Has Been Peformed!", TrayIcon.MessageType.INFO);
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

    static class aa extends JWindow {

        public aa() {
            this.getContentPane().setLayout(new BorderLayout());
            JLabel aa=new JLabel("sdfsdsdf");
           final JPopupMenu pm =new JPopupMenu();
            JMenuItem jmi = new JMenuItem("sdsdsds");
            pm.add(jmi);
            this.getContentPane().add(pm);
            this.getContentPane().add(aa, BorderLayout.CENTER);
            pack();
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    setLocation(500,500);
                    setVisible(true);
                    pm.show();
                }
            });
        }
    }
}