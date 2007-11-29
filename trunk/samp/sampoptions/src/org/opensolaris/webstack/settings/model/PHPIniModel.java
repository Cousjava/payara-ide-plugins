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
public class PHPIniModel extends Model {

    private static final String ERROR_REPORTING="error_reporting  =";
    int errorreportingKey = -1;

    private boolean changed = false;
    private String ErrorReporting;

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

        ErrorReporting = "";

        changed=false;
        load();
    }

    @Override
    public void lineAddedCallBack(String line, int lineNumber) {
        if (line.startsWith(ERROR_REPORTING)) {
            errorreportingKey = lineNumber;
            ErrorReporting=  line.substring(ERROR_REPORTING.length(), line.length()).trim();
        }


    }
    public String getErrorReporting(){
        return ErrorReporting;
        
    }
    public void setErrorReporting(String e){
        System.out.println("setting error to:"+e);
        
    }
    public boolean isDirty() {
        return changed;
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