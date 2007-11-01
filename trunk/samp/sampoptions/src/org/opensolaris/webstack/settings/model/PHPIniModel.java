/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opensolaris.webstack.settings.model;

import java.io.File;

/**
 *
 * @author ludo
 */
public class PHPIniModel extends Model {
    private static final String REMOTEMODE = "xdebug.remote_mode=req";
    private static final String REMOTEENABLE = "xdebug.remote_enable=1";
    private static final String REMOTEHANDLER = "xdebug.remote_handler=dbgp";
    private static final String REMOTEHOST = "xdebug.remote_host=localhost";

    int errorreportingKey = -1;
    int xdebugKey = -1;
    int zend_extensionKey = -1;
    int xdebugremote_enableKey = -1;
    int xdebugremote_modeKey = -1;
    int xdebugremote_hostKey = -1;
    int xdebugremote_handlerKey = -1;
    
    private boolean initialDebugMode;
    private boolean changed = false;

    public PHPIniModel() {
        super(new File(Environment.getPhpini()));
    }
    /* 
    reset is called before reading the file after timestamp changes
     * override to reset your private state data if needed
     * default impl is doing a reload from file on disk
     */

    @Override
    public void reset() {
        errorreportingKey = -1;
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
        if (line.startsWith("error_reporting")) {
            errorreportingKey = lineNumber;
        } else if (line.startsWith("error_reporting")) {
            errorreportingKey = lineNumber;
        } else if (line.startsWith("zend_extension")) {
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
        System.out.println("in debug called xdebugremote_enableKey="+xdebugremote_enableKey);
        return (xdebugremote_enableKey != -1);// this key is present in the file
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