/*
 * OptionsContainer.java
 *
 * Created on September 20, 2007, 4:23 PM
 */

package samp.options;

import java.awt.Image;
import java.net.URL;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

/**
 *
 * @author  ludo
 */
public class OptionsContainer extends javax.swing.JFrame {
    private static OptionsContainer optionsContainer =null;
    
    public static OptionsContainer getInstance(){
        if (optionsContainer==null){
            optionsContainer = new OptionsContainer();
        }
        return optionsContainer;
        
    }
    /** Creates new form OptionsContainer */
    private OptionsContainer() {
        initComponents();
        tabsPanel.addTab("General", new GeneralPanel());
        tabsPanel.addTab("Apache 2", new Apache2Panel());
        tabsPanel.addTab("PHP", new PHPPanel());
        tabsPanel.addTab("MySQL", new MySQLPanel());
        tabsPanel.addTab("FTP", new FTPPanel());
        
       
        pack();
        setDefaultCloseOperation(javax.swing.JFrame.DISPOSE_ON_CLOSE);
            URL url = samp.tray.Tray.class.getResource("resources/gnome-html.png");

            Image image = new ImageIcon(url).getImage();        setIconImage(image);
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        titleLabel = new JLabel(new ImageIcon(samp.tray.Tray.class.getResource("resources/open.png")));

        ;
        tabsPanel = new javax.swing.JTabbedPane();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("samp/tray/Bundle"); // NOI18N
        setTitle(bundle.getString("LABEL_Title")); // NOI18N

        tabsPanel.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        tabsPanel.setMinimumSize(new java.awt.Dimension(0, 0));

        okButton.setText(bundle.getString("LABEL_OK")); // NOI18N
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });

        cancelButton.setText(bundle.getString("LABEL_Cancel")); // NOI18N
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(okButton)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 350, Short.MAX_VALUE)
                        .addComponent(cancelButton))
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(titleLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 431, Short.MAX_VALUE)
                            .addComponent(tabsPanel, javax.swing.GroupLayout.DEFAULT_SIZE, 431, Short.MAX_VALUE))))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(titleLabel, javax.swing.GroupLayout.DEFAULT_SIZE, 60, Short.MAX_VALUE)
                .addGap(18, 18, 18)
                .addComponent(tabsPanel, javax.swing.GroupLayout.PREFERRED_SIZE, 273, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cancelButton)
                    .addComponent(okButton))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        this.dispose();
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        this.dispose();
    }//GEN-LAST:event_okButtonActionPerformed
    

    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton okButton;
    private javax.swing.JTabbedPane tabsPanel;
    private javax.swing.JLabel titleLabel;
    // End of variables declaration//GEN-END:variables
    
}
