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
 */
/*
 * Copyright 2007 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */
package org.opensolaris.webstack.settings.options;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Paint;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import org.opensolaris.webstack.settings.model.HttpdConfModel;
import org.opensolaris.webstack.settings.model.MySQLCnfModel;
import org.opensolaris.webstack.settings.model.PHPIniModel;
import org.opensolaris.webstack.settings.tray.Main;

/**
 *
 * @author  ludo
 */
public class OptionsContainer extends javax.swing.JFrame {

    private Apache2Panel apacheTab;
    private PHPPanel phpTab;
    private HttpdConfModel model;
    private PHPIniModel phpmodel;
    private MySQLCnfModel mysqlmodel;

    /** Creates new form OptionsContainer */
    public OptionsContainer(final HttpdConfModel model) {
        this.model = model;
        phpmodel = new PHPIniModel();
        mysqlmodel =new MySQLCnfModel();
        initComponents();
        //     tabsPanel.addTab("General", new GeneralPanel());
        tabsPanel.addTab("Apache 2", apacheTab = new Apache2Panel(model));
        tabsPanel.addTab("PHP", phpTab = new PHPPanel(phpmodel));
        tabsPanel.addTab("MySQL", new MySQLPanel(mysqlmodel));
        tabsPanel.addTab("FTP", new FTPPanel());


        pack();
        setLocationRelativeTo(null);
        URL url = Main.class.getResource("resources/apache.gif");

        Image image = new ImageIcon(url).getImage();
        setIconImage(image);
        setDefaultCloseOperation(javax.swing.JFrame.DISPOSE_ON_CLOSE);
        addWindowListener(new WindowListener() {

            public void windowOpened(WindowEvent arg0) {
            }

            public void windowClosing(WindowEvent arg0) {
                saveChanges();
            }

            public void windowClosed(WindowEvent arg0) {
            //   throw new UnsupportedOperationException("Not supported yet.");
            }

            public void windowIconified(WindowEvent arg0) {
            //   throw new UnsupportedOperationException("Not supported yet.");
            }

            public void windowDeiconified(WindowEvent arg0) {
            //   throw new UnsupportedOperationException("Not supported yet.");
            }

            public void windowActivated(WindowEvent arg0) {
            //  throw new UnsupportedOperationException("Not supported yet.");
            }

            public void windowDeactivated(WindowEvent arg0) {
            //   throw new UnsupportedOperationException("Not supported yet.");
            }
        });

    }

    private void saveChanges() {
        System.out.println(" save is CALLED " + model.isDirty());


        apacheTab.UpdateModel();
        phpTab.UpdateModel();
        boolean apachedirtyModel = model.isDirty();
        boolean phpdirtyModel = phpmodel.isDirty();
        if (model.isDirty()) {
            model.save();
        }

        if (phpdirtyModel) {
            phpmodel.save();
        }
        if (apachedirtyModel || phpdirtyModel) {
            Object[] options = {"Retart Servers Now", "Close"};
            int n = JOptionPane.showOptionDialog(null, "You must Restart Servers for your changes to take effect. ", "Change Options", JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[1]);
        }

        this.dispose();
    }

    private JLabel getImage() {
        BufferedImage image = null;
        try {
            image = ImageIO.read(org.opensolaris.webstack.settings.tray.Tray.class.getResource("resources/samp.gif"));
        // image = createReflection(image);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return new JLabel(new ImageIcon(image));
    }

    private BufferedImage createReflection(BufferedImage image) {
        int height = image.getHeight();

        BufferedImage result = new BufferedImage(image.getWidth(), height * 2, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = result.createGraphics();

        // Paints original image
        g2.drawImage(image, 0, 0, null);

        // Paints mirrored image
        g2.scale(1.0, -1.0);
        g2.drawImage(image, 0, -height - height, null);
        g2.scale(1.0, -1.0);

        // Move to the origin of the clone
        g2.translate(0, height);

        // Creates the alpha mask
        GradientPaint mask;
        mask = new GradientPaint(0, 0, new Color(1.0f, 1.0f, 1.0f, 0.5f), 0, height / 2, new Color(1.0f, 1.0f, 1.0f, 0.0f));
        Paint oldPaint = g2.getPaint();
        g2.setPaint(mask);

        // Sets the alpha composite
        g2.setComposite(AlphaComposite.DstIn);

        // Paints the mask
        g2.fillRect(0, 0, image.getWidth(), height);

        g2.dispose();
        return result;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        titleLabel = getImage() ;
        ;
        tabsPanel = new javax.swing.JTabbedPane();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("org/opensolaris/webstack/settings/tray/Bundle"); // NOI18N
        setTitle(bundle.getString("LABEL_Title")); // NOI18N

        tabsPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        tabsPanel.setMinimumSize(new java.awt.Dimension(0, 0));

        java.util.ResourceBundle bundle1 = java.util.ResourceBundle.getBundle("org/opensolaris/webstack/settings/options/Bundle"); // NOI18N
        okButton.setText(bundle1.getString("LABEL_OK")); // NOI18N
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });

        cancelButton.setText(bundle1.getString("LABEL_Cancel")); // NOI18N
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(titleLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 521, Short.MAX_VALUE)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(okButton)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 438, Short.MAX_VALUE)
                .addComponent(cancelButton)
                .addContainerGap())
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(tabsPanel, javax.swing.GroupLayout.DEFAULT_SIZE, 497, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(titleLabel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(tabsPanel, javax.swing.GroupLayout.DEFAULT_SIZE, 304, Short.MAX_VALUE)
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cancelButton)
                    .addComponent(okButton))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        //reset the model from disk
        // thsi will update the internal values displayed in the UI to the correct values
        model.reset();
        phpmodel.reset();
        this.dispose();
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        saveChanges();

    }//GEN-LAST:event_okButtonActionPerformed
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton okButton;
    private javax.swing.JTabbedPane tabsPanel;
    private javax.swing.JLabel titleLabel;
    // End of variables declaration//GEN-END:variables
}
