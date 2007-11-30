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
package org.opensolaris.webstack.settings.tray;

import java.beans.PropertyChangeListener;
import java.util.Timer;
import java.util.TimerTask;
import java.beans.PropertyChangeSupport;
import org.opensolaris.webstack.settings.execution.ServerStatus;
import org.opensolaris.webstack.settings.execution.ServersManager;

/**
 *
 * @author Ludo
 */
public class StatusThread {

    Timer timer;
    static private int delay = 0;   // delay for 0 sec.
    static private int period = 5000;  // repeat every 5 sec.
    PropertyChangeSupport changeSupport;
    private ServerStatus currentStatus;

    public StatusThread(final Tray t) {
        currentStatus = ServersManager.getRunningState();
        changeSupport = new PropertyChangeSupport(this);

        timer = new Timer(true); //deamon
        timer.scheduleAtFixedRate(new CheckChangeTask(t), delay, period);

    }

    public void addPropertyChangeListener(PropertyChangeListener l) {
        changeSupport.addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
        changeSupport.removePropertyChangeListener(l);
    }

    class CheckChangeTask extends TimerTask {

        Tray tray;

        public CheckChangeTask(Tray t) {
            tray = t;

        }

        public void run() {
            final ServerStatus running = ServersManager.getRunningState();

            if ((currentStatus.apacheRunning != running.apacheRunning) ||
                    (currentStatus.mySqlRunning != running.mySqlRunning)) {
                tray.updateIcon();
            }
            currentStatus=running;


            changeSupport.firePropertyChange("timeStamp", "a", "");
        }
        }
}
