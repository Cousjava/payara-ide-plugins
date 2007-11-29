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

import java.io.File;

/**
 *
 * @author ludo
 */
public class XdebugIniModel extends Model {
    private static final String REMOTEMODE = "xdebug.remote_mode=req";
    private static final String REMOTEENABLE = "xdebug.remote_enable=1";
    private static final String REMOTEHANDLER = "xdebug.remote_handler=dbgp";
    private static final String REMOTEHOST = "xdebug.remote_host=localhost";
    int xdebugKey = -1;
    int zend_extensionKey = -1;
    int xdebugremote_enableKey = -1;
    int xdebugremote_modeKey = -1;
    int xdebugremote_hostKey = -1;
    int xdebugremote_handlerKey = -1;
    
    private boolean initialDebugMode;
    private boolean changed = false;

    public XdebugIniModel() {
        super(new File(Environment.getXdebugini()));
    }
    /* 
    reset is called before reading the file after timestamp changes
     * override to reset your private state data if needed
     * default impl is doing a reload from file on disk
     */

    @Override
    public void reset() {
        xdebugKey = -1;
        zend_extensionKey = -1;
        xdebugremote_enableKey = -1;
        xdebugremote_modeKey = -1;
        xdebugremote_hostKey = -1;
        xdebugremote_handlerKey = -1;
        initialDebugMode =false;
        changed=false;
        load();
    }

    @Override
    public void lineAddedCallBack(String line, int lineNumber) {
        if (line.startsWith("zend_extension")) {
            zend_extensionKey = lineNumber;
        } else if (line.startsWith("xdebug.remote_enable")) {
            initialDebugMode =true;//debug is on in the file
            xdebugremote_enableKey = lineNumber;
        } else if (line.startsWith("xdebug.remote_mode")) {
            xdebugremote_modeKey = lineNumber;
        } else if (line.startsWith("xdebug.remote_host")) {
            xdebugremote_hostKey = lineNumber;
        } else if (line.startsWith("xdebug.remote_handler")) {
            xdebugremote_handlerKey = lineNumber;
        }



    }

    public boolean isDirty() {
        return changed;
    }

    public boolean isDebugMode() {
      //  System.out.println("in debug called xdebugremote_enableKey="+xdebugremote_enableKey);
        return (zend_extensionKey != -1);// this key is present in the file
    }

    public void setDebugMode(boolean debugmode) {
        if (initialDebugMode==debugmode){
            return;//nothing changed no need to updae the file on disk
        }
        changed = true;
        if (debugmode) {
            if (zend_extensionKey != -1) {
                content.set(zend_extensionKey, "zend_extension=/usr/php5/5.2.4/modules/xdebug.so");
            } else {
                content.add("zend_extension=/usr/php5/5.2.4/modules/xdebug.so");

            }
            if (xdebugremote_enableKey != -1) {
                content.set(xdebugremote_enableKey,REMOTEENABLE);
            } else {
                content.add(REMOTEENABLE);

            }
            if (xdebugremote_handlerKey != -1) {
                content.set(xdebugremote_handlerKey,REMOTEHANDLER);
            } else {
                content.add(REMOTEHANDLER);

            }

            if (xdebugremote_modeKey != -1) {
                content.set(xdebugremote_modeKey,REMOTEMODE);
            } else {
                content.add(REMOTEMODE);

            }
            if (xdebugremote_hostKey != -1) {
                content.set(xdebugremote_hostKey,REMOTEHOST);
            } else {
                content.add(REMOTEHOST);

            }

        } else {
            if (xdebugremote_hostKey != -1) {
                content.remove(xdebugremote_hostKey);
            }
            if (xdebugremote_modeKey != -1) {
                content.remove(xdebugremote_modeKey);
            }
            if (xdebugremote_handlerKey != -1) {
                content.remove(xdebugremote_handlerKey);
            }
            if (xdebugremote_enableKey != -1) {
                content.remove(xdebugremote_enableKey);
            }
            if (zend_extensionKey != -1) {
                content.remove(zend_extensionKey);
            }

        }

    }
}
/*
 * 
 * [xdebug]
zend_extension=/Applications/MAMP/bin/php5/lib/php/extensions/no-debug-non-zts-20050922/xdebug.so
xdebug.remote_enable=1
xdebug.remote_handler=dbgp
xdebug.remote_mode=req
xdebug.remote_host=localhost
 * */