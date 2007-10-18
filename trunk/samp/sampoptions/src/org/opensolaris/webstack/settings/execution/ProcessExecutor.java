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

package org.opensolaris.webstack.settings.execution;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ludo
 */
public class ProcessExecutor {

    static ExecutorService pool = Executors.newFixedThreadPool(1); //1 process at a time

    public static void main() {

        for (int i = 0; i < 10; i++) {
            final int j = i;
            Future f = pool.submit(new Runnable() {

                public void run() {
                    try {
                        Thread.sleep(1000 / j);
                        System.out.println("i" + j);
                    } catch (InterruptedException ex) {
                        Logger.getLogger(ProcessExecutor.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            });
            if (i == 3) {
                f.cancel(true);
            }
        }
    }

/** This thread simply reads from given Reader and writes read chars to given Writer. */
    public static class OutputCopier extends Thread {

        final Writer os;
        final Reader is;
        /** while set to false at streams that writes to the display it must be
         * true for a stream that reads from the window.
         */
        final boolean autoflush;
        private boolean done = false;

        public OutputCopier(Reader is, Writer os, boolean b) {
            this.os = os;
            this.is = is;
            autoflush = b;
        }

        /* Makes copy. */
        public void run() {
            int read;
            char[] buff = new char[256];
            try {
                while ((read = read(is, buff, 0, 256)) > 0x0) {
                    if (os != null) {
                        os.write(buff, 0, read);
                        if (autoflush) {
                            os.flush();
                        }
                    }
                }
            } catch (IOException ex) {
            } catch (InterruptedException e) {
            }
        }

        public void interrupt() {
            super.interrupt();
            done = true;
        }

        private int read(Reader is, char[] buff, int start, int count) throws InterruptedException, IOException {

            while (!is.ready() && !done) {
                sleep(100);
            }
            return is.read(buff, start, count);
        }
    }
}