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

package org.opensolaris.webstack.settings.options;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.border.EmptyBorder;

/**
 *
 * @author Ludo
 */
public class HyperLinkButton extends JButton implements MouseListener, ActionListener, FocusListener {

    private static final int FONT_SIZE = 12; // Utils.getDefaultFontSize();
    private static final Font BUTTON_FONT = new Font(null, Font.BOLD, FONT_SIZE);
    private static final Stroke LINK_IN_FOCUS_STROKE = new BasicStroke(1, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_BEVEL, 0, new float[]{0, 2}, 0);
    private static final String LinkInFocusColor = "0x000000"; //NOI18N
    private static final String LinkColor = "0x164B7B"; //NOI18N
    private boolean underline = false;
    private String url;

    public HyperLinkButton(String label, String url) {
        super(label);
        this.url = url;
        setForeground(getColor(LinkColor));
        setFont(BUTTON_FONT);
        setBorder(new EmptyBorder(1, 1, 1, 1));
        setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        setHorizontalAlignment(JLabel.LEFT);
        addMouseListener(this);
        setFocusable(true);
        setMargin(new Insets(0, 0, 0, 0));
        setBorderPainted(false);
        setFocusPainted(false);
        setRolloverEnabled(true);
        setContentAreaFilled(false);

        addActionListener(this);
        addFocusListener(this);
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseClicked(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
        if (isEnabled()) {
            underline = true;
            setForeground(getColor(LinkInFocusColor));
            repaint();
            onMouseEntered(e);
        }
    }

    public void mouseExited(MouseEvent e) {
        if (isEnabled()) {
            underline = false;
            setForeground(getColor(LinkColor));
            repaint();
            onMouseExited(e);
        }
    }

    public Color getColor(String resId) {
        try {
            Integer rgb = Integer.decode(resId);
            return new Color(rgb.intValue());
        } catch (NumberFormatException nfE) {
            return Color.BLACK;
        }
    }

    public static Graphics2D prepareGraphics(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
        Map rhints = (Map) (Toolkit.getDefaultToolkit().getDesktopProperty("awt.font.desktophints")); //NOI18N
        if (rhints == null && Boolean.getBoolean("swing.aatext")) {
            //NOI18N
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        } else if (rhints != null) {
            g2.addRenderingHints(rhints);
        }
        return g2;
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = prepareGraphics(g);
        super.paintComponent(g2);

        if (hasFocus() && isEnabled()) {
            Dimension size = getSize();
            g2.setStroke(LINK_IN_FOCUS_STROKE);
            g2.setColor(getColor(LinkInFocusColor));
            g2.drawRect(0, 0, size.width - 1, size.height - 1);
        }
    }

    public void focusGained(FocusEvent e) {
        Rectangle rect = getBounds();
        rect.grow(0, FONT_SIZE);
        scrollRectToVisible(rect);
    }

    public void focusLost(FocusEvent e) {
    }

    protected void onMouseEntered(MouseEvent e) {
    }

    protected void onMouseExited(MouseEvent e) {
    }

    public void actionPerformed(ActionEvent e) {
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
                URI uri = new URI(getText());
                desktop.browse(uri);
            } catch (IOException ex) {
                Logger.getLogger(Apache2Panel.class.getName()).log(Level.SEVERE, null, ex);
            } catch (URISyntaxException ex2) {
                Logger.getLogger(Apache2Panel.class.getName()).log(Level.SEVERE, null, ex2);
            }
        }
    }

    @Override
    public void paint(Graphics g) {
        super.paint(g);
        if (underline && isEnabled()) {
            Font f = getFont();
            FontMetrics fm = getFontMetrics(f);
            int iconWidth = 0;
            if (null != getIcon()) {
                iconWidth = getIcon().getIconWidth() + getIconTextGap();
            }
            int x1 = iconWidth;
            int y1 = fm.getHeight();
            int x2 = fm.stringWidth(getText()) + iconWidth;
            if (getText().length() > 0) {
                g.drawLine(x1, y1, x2, y1);
            }
        }
    }
}