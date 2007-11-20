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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ludo
 */
public class Model {

    File modelFile = null;
    ArrayList<String> content ;
    long timeStamp = 0;
    Timer timer;
    static private int delay = 0;   // delay for 0 sec.
    static private int period = 3000;  // repeat every 3 sec.
    PropertyChangeSupport changeSupport;

    public Model(File modelFile) {
        this.modelFile = modelFile;
        changeSupport = new PropertyChangeSupport(this);
        timer = new Timer(true); //deamon
        timer.scheduleAtFixedRate(new CheckChangeTask(modelFile), delay, period);

    }

    public void addPropertyChangeListener(PropertyChangeListener l) {
        changeSupport.addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
        changeSupport.removePropertyChangeListener(l);
    }

    public void lineAddedCallBack(String line, int lineNumber) {

    }

    /* 
    reset is called before reading the file after timestamp changes
     * override to reset your private state data if needed
     * default impl is doing a reload from file on disk
     */
    public void reset() {
        load();
    }

    public void load() {
        FileInputStream fis = null;
        BufferedReader br = null;
        content = new ArrayList();
        timeStamp = 0;
        try {
            fis = new FileInputStream(modelFile);

            // read the config from resource first
            br = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
            String line = br.readLine();
            int i = 0;
            System.out.println(" loading the file" + modelFile.getAbsolutePath());
            while (line != null) {
                content.add(line);
                lineAddedCallBack(line, i);

                i++;


                line = br.readLine();
            }
        } catch (Exception ex) {
            Logger.getLogger(Model.class.getName()).log(Level.SEVERE, ex.getLocalizedMessage(), (Throwable)null);
        } finally {
            try {
                if (br != null) {
                    br.close();
                }
            } catch (IOException ex) {
                Logger.getLogger(Model.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

    }
    public boolean isDirty() {
        return false;
    }
    public void save() {

        FileWriter fout = null;

        try {
            fout = new FileWriter(modelFile,false);
            for (int i = 0; i < content.size(); i++) {
                fout.write(content.get(i)+"\n");
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fout.close();
            } catch (IOException ex) {
                Logger.getLogger(Model.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    class CheckChangeTask extends TimerTask {

        final File modelFile;

        public CheckChangeTask(File f) {
            modelFile = f;

        }

        public void run() {
            long currentTimeStamp = modelFile.lastModified();
            if (timeStamp < currentTimeStamp) {
//                System.out.println("timeStamp < currentTimeStamp"+timeStamp +"---"+ currentTimeStamp);
//                System.out.println("modelFile"+modelFile);
                reset();
//                System.out.println("time stamp changed, reloading");
                long old = timeStamp;
                timeStamp = modelFile.lastModified();
                changeSupport.firePropertyChange("timeStamp", "" + old, "" + timeStamp);

            }
        }
    }
}
