/*
 * Util.java
 *
 * Created on Sep 27, 2007, 8:30:29 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.model;

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
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ludo
 */
public class Util {

    private void updateFile(File target, String content) throws IOException {
        try {
            OutputStream os = new FileOutputStream(target);
            BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(os, "UTF-8"));
            bw.write(content);
            bw.close();
        } finally {
        }
    }

    public static String readAndReplace(InputStream is, String oldStartLine, String newLine) throws IOException {
        // read the config from resource first
        StringBuffer sb = new StringBuffer();
        String lineSep = System.getProperty("line.separator"); //NOI18N
        BufferedReader br = new BufferedReader(new InputStreamReader(is, "UTF-8"));
        String line = br.readLine();


        while (line != null) {
            if (line.startsWith(oldStartLine)) {
                line = newLine;
            }

            sb.append(line);
            sb.append(lineSep);
            line = br.readLine();
        }

        br.close();

        return sb.toString();
    }

    public static String getValue(File confFile, String key) {
        FileInputStream fis = null;
        BufferedReader br = null;
        try {
            fis = new FileInputStream(confFile);
            // read the config from resource first
            br = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
            String line = br.readLine();


            while (line != null) {
                if (line.startsWith(key)) {
                    System.out.println(" line is " + line);
                    return line.substring(key.length(), line.length());
                }

                line = br.readLine();
            }
        } catch (Exception ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            try {
                if (br != null) {
                    br.close();
                }
            } catch (IOException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return "80";
    }

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