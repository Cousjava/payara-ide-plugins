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

package org.opensolaris.webstack.settings.model;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.Transparency;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;

/**
 *
 * @author ludo
 */
public class Util {





    /**
     * Method that attempts to find the merged image in the cache first, then
     * creates the image if it was not found.
     *  @param x x position of top-left corner
     * @param y y position of top-left corner
     */
    public static final Image mergeImages(Image im1, Image im2, int x, int y) {



        return doMergeImages(im1, im2, x, y);
    }

    private static final Image doMergeImages(Image image1, Image image2, int x, int y) {
        ensureLoaded(image1);
        ensureLoaded(image2);
        int w = Math.max(image1.getWidth(null), x + image2.getWidth(null));
        int h = Math.max(image1.getHeight(null), y + image2.getHeight(null));
        boolean bitmask = (image1 instanceof Transparency) && ((Transparency) image1).getTransparency() != Transparency.TRANSLUCENT && (image2 instanceof Transparency) && ((Transparency) image2).getTransparency() != Transparency.TRANSLUCENT;

        ColorModel model = colorModel(bitmask ? Transparency.BITMASK : Transparency.TRANSLUCENT);
        BufferedImage buffImage = new BufferedImage(model, model.createCompatibleWritableRaster(w, h), model.isAlphaPremultiplied(), null);

        java.awt.Graphics g = buffImage.createGraphics();
        g.drawImage(image1, 0, 0, null);
        g.drawImage(image2, x, y, null);
        g.dispose();

        return buffImage;
    }

    private static ColorModel colorModel(int transparency) {
        ColorModel model;
        try {
            model = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration().getColorModel(transparency);
        } catch (HeadlessException he) {
            model = ColorModel.getRGBdefault();
        }

        return model;
    }
    private static final Component component = new Component() {
    };
    private static final MediaTracker tracker = new MediaTracker(component);
    private static int mediaTrackerID;

    private static void ensureLoaded(Image image) {
        if ((Toolkit.getDefaultToolkit().checkImage(image, -1, -1, null) & (ImageObserver.ALLBITS | ImageObserver.FRAMEBITS)) != 0) {
            return;
        }

        synchronized (tracker) {
            int id = ++mediaTrackerID;

            tracker.addImage(image, id);

            try {
                tracker.waitForID(id, 0);
            } catch (InterruptedException e) {
                System.out.println("INTERRUPTED while loading Image");
            }

            assert (tracker.statusID(id, false) == MediaTracker.COMPLETE) : "Image loaded";
            tracker.removeImage(image, id);
        }
    }
}