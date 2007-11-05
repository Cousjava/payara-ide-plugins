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
public class MySQLCnfModel extends Model {
    private static final String SKIPNETWORKING = "skip-networking";


    int skipnetworkingKey = -1;


    private boolean changed = false;

    public MySQLCnfModel() {
        super(new File(Environment.getPhpini()));
    }
    /* 
    reset is called before reading the file after timestamp changes
     * override to reset your private state data if needed
     * default impl is doing a reload from file on disk
     */

    @Override
    public void reset() {
        skipnetworkingKey = -1;

        changed=false;
        load();
    }

    @Override
    public void lineAddedCallBack(String line, int lineNumber) {
        if (line.startsWith(SKIPNETWORKING)) {
            skipnetworkingKey = lineNumber;
        } 


    }

    public boolean isDirty() {
        return changed;
    }

    public boolean isSkipNetworking() {
        return (skipnetworkingKey != -1);// this key is present in the file
    }

    public void setSkipNetworking(boolean skipnetworking) {
//        if (initialDebugMode==debugmode){
//            return;//nothing changed no need to updae the file on disk
//        }
        changed = true;
        if (skipnetworking) {
            if (skipnetworkingKey != -1) {
                content.set(skipnetworkingKey, SKIPNETWORKING);
            } else {
                content.add(SKIPNETWORKING);

            }


        } else {
            if (skipnetworkingKey != -1) {
                content.remove(skipnetworkingKey);
            }


        }

    }
}
