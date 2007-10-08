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
 */ /*
 * Copyright 2007 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */

package samp.options;

import java.awt.Desktop;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;
import samp.model.Environment;

/**
 *
 * @author  ludo
 */
public class Apache2Panel extends javax.swing.JPanel {

    /** Creates new form Apache2Panel */
    public Apache2Panel() {
        initComponents();
        textFieldPortNumber.setText(Environment.getApachePortNumber());
        textFieldPortNumber.addKeyListener(new KeyAdapter() {

            public void keyTyped(KeyEvent e) {
                char c = e.getKeyChar();
                if (!((Character.isDigit(c) || (c == KeyEvent.VK_BACK_SPACE) || (c == KeyEvent.VK_DELETE)))) {
                    getToolkit().beep();
                    e.consume();
                }
            }
        });
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        bindingGroup = new org.jdesktop.beansbinding.BindingGroup();

        labelPortNumber = new javax.swing.JLabel();
        textFieldPortNumber = new javax.swing.JTextField();
        labelDocRoot = new javax.swing.JLabel();
        textFieldDocRoot = new javax.swing.JTextField();
        labelWebSite = new javax.swing.JLabel();
        buttonOpen = new javax.swing.JButton();
        jCheckBox1 = new javax.swing.JCheckBox();
        jLabel1 = new javax.swing.JLabel();
        buttonAdvanceConf = new javax.swing.JButton();
        buttonRepair = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        labelWebPage = new javax.swing.JLabel();

        labelPortNumber.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        labelPortNumber.setLabelFor(textFieldPortNumber);
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("samp/options/Bundle"); // NOI18N
        labelPortNumber.setText(bundle.getString("LABEL_PORT")); // NOI18N

        textFieldPortNumber.setText("80");

        labelDocRoot.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        labelDocRoot.setLabelFor(textFieldDocRoot);
        labelDocRoot.setText(bundle.getString("LABEL_DOCDIR")); // NOI18N

        textFieldDocRoot.setText("/usr/apache2/htdocs");

        labelWebSite.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        labelWebSite.setText(bundle.getString("LABEL_HOMEPAGE")); // NOI18N

        buttonOpen.setText(bundle.getString("LABEL_OPEN")); // NOI18N
        buttonOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                buttonOpenActionPerformed(evt);
            }
        });

        jCheckBox1.setText(bundle.getString("LABEL_HOMEDIRS")); // NOI18N

        jLabel1.setText(bundle.getString("LABEL_HELP")); // NOI18N
        jLabel1.setEnabled(false);

        buttonAdvanceConf.setText(bundle.getString("LABEL_ADVANCE_CONF")); // NOI18N
        buttonAdvanceConf.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                buttonAdvanceConfActionPerformed(evt);
            }
        });

        buttonRepair.setText(bundle.getString("LABEL_REPAIR")); // NOI18N

        jLabel2.setText(bundle.getString("LABEL_EDITHTTPD")); // NOI18N
        jLabel2.setEnabled(false);

        org.jdesktop.beansbinding.Binding binding = org.jdesktop.beansbinding.Bindings.createAutoBinding(org.jdesktop.beansbinding.AutoBinding.UpdateStrategy.READ_WRITE, textFieldPortNumber, org.jdesktop.beansbinding.ELProperty.create("http://localhost:${text}"), labelWebPage, org.jdesktop.beansbinding.BeanProperty.create("text"));
        bindingGroup.addBinding(binding);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addComponent(labelDocRoot, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(labelPortNumber, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 135, Short.MAX_VALUE))
                    .addComponent(labelWebSite))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBox1, javax.swing.GroupLayout.DEFAULT_SIZE, 264, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, 252, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(buttonAdvanceConf)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel2, javax.swing.GroupLayout.DEFAULT_SIZE, 112, Short.MAX_VALUE))
                    .addComponent(buttonRepair)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(labelWebPage, javax.swing.GroupLayout.DEFAULT_SIZE, 203, Short.MAX_VALUE)
                        .addGap(18, 18, 18)
                        .addComponent(buttonOpen))
                    .addComponent(textFieldDocRoot, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 264, Short.MAX_VALUE)
                    .addComponent(textFieldPortNumber, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 264, Short.MAX_VALUE))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {labelDocRoot, labelPortNumber, labelWebSite});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(labelPortNumber)
                    .addComponent(textFieldPortNumber, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(textFieldDocRoot, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(labelDocRoot))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(buttonOpen)
                    .addComponent(labelWebSite)
                    .addComponent(labelWebPage))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBox1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(buttonAdvanceConf)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(buttonRepair)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        bindingGroup.bind();
    }// </editor-fold>//GEN-END:initComponents

    private void buttonAdvanceConfActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonAdvanceConfActionPerformed

        Desktop desktop = null;
        // Before more Desktop API is used, first check
        // whether the API is supported by this particular
        // virtual machine (VM) on this particular host.
        if (Desktop.isDesktopSupported()) {
            desktop = Desktop.getDesktop();
        }
        if (desktop.isSupported(Desktop.Action.OPEN)) {
            try {

                desktop.open(new File(Environment.getHttpdconf()));
            } catch (IOException ex) {
                Logger.getLogger(Apache2Panel.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }//GEN-LAST:event_buttonAdvanceConfActionPerformed

    private void buttonOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonOpenActionPerformed
        Desktop desktop = null;
        // Before more Desktop API is used, first check
        // whether the API is supported by this particular
        // virtual machine (VM) on this particular host.
        if (Desktop.isDesktopSupported()) {
            desktop = Desktop.getDesktop();
        }
        if (desktop.isSupported(Desktop.Action.BROWSE)) {
            try {
                // launch browser
                URI uri = new URI(labelWebPage.getText());
                desktop.browse(uri);
            } catch (IOException ex) {
                Logger.getLogger(Apache2Panel.class.getName()).log(Level.SEVERE, null, ex);
            } catch (URISyntaxException ex2) {
                Logger.getLogger(Apache2Panel.class.getName()).log(Level.SEVERE, null, ex2);
            }
        }
    }//GEN-LAST:event_buttonOpenActionPerformed
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton buttonAdvanceConf;
    private javax.swing.JButton buttonOpen;
    private javax.swing.JButton buttonRepair;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel labelDocRoot;
    private javax.swing.JLabel labelPortNumber;
    private javax.swing.JLabel labelWebPage;
    private javax.swing.JLabel labelWebSite;
    private javax.swing.JTextField textFieldDocRoot;
    private javax.swing.JTextField textFieldPortNumber;
    private org.jdesktop.beansbinding.BindingGroup bindingGroup;
    // End of variables declaration//GEN-END:variables
}