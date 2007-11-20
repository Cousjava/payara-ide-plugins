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
