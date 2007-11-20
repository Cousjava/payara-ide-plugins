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
public class HttpdConfModel extends Model {

    
    /* variables storing the line number in the file of various keys
     * 0 otherwise if not in the file
     * */
    int listenKey = 0;
    int documentRootKey = 0;
    int directorydocumentRootKey = 0 ;//key for line  <Directory "/xxx/htdocs">
    int userdirKey = 0;
    int portNumber = 80;
    String docRoot = "";
    
    /* value read from the file, before possible user modification
     * */
    
    String initialDocRoot;
    boolean initiaUserDirEnable=false;
    
    
    boolean changed = false;
    static private String LISTEN = "Listen";// port key
    static private String DOCROOT = "DocumentRoot";// root key
    static private String USERDIR="UserDir";  //user dir key
    static private String DIRECTORY="<Directory";  //dir dir key for the docroot configuration only
    public HttpdConfModel() {
        super(new File(Environment.getHttpdconf()));
    }

    @Override
    public void reset() {
        initialDocRoot="";
        initiaUserDirEnable=false;
        documentRootKey=-1;
        directorydocumentRootKey=-1;
        listenKey=-1;
        
         changed = false;
        load();
    }

    @Override
    public void lineAddedCallBack(String line, int lineNumber) {
        if (line.startsWith(LISTEN)) {
            listenKey = lineNumber;

            try {
                portNumber = Integer.parseInt((line.substring(LISTEN.length(), line.length())).trim());
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        if (line.startsWith(USERDIR)) {
            userdirKey = lineNumber;
            initiaUserDirEnable =true;
        }
        if (line.startsWith(DOCROOT)) {
            documentRootKey = lineNumber;
            docRoot = line.substring(DOCROOT.length(), line.length()).trim();
            if (docRoot.startsWith("\"")) {
                docRoot = docRoot.substring(1);
            }
            if (docRoot.endsWith("\"")) {
                docRoot = docRoot.substring(0, docRoot.length() - 1);
            }
                initialDocRoot = docRoot;
        }
        if (line.startsWith(DIRECTORY)) {
                //something like <Directory "/Applications/MAMP/htdocs">
                //test if this dir is the real doc root dir:
            String dir = line.substring(DOCROOT.length(), line.length()).trim();
            if (dir.endsWith(">")) {
                dir = dir.substring(0, dir.length() - 1);
                
            }
            if (dir.startsWith("\"")) {
                dir = dir.substring(1);
            }
            if (dir.endsWith("\"")) {
                dir = dir.substring(0, dir.length() - 1);                
            }
            if (dir.equals(docRoot)){// this is the entry we are looking for
                directorydocumentRootKey = lineNumber;
            }
        }
    }

    public int getPortNumber() {
        return portNumber;


    }

    public void setPortNumber(int i) {

        if (changed == false) {
            changed = (portNumber != i);
        }
        System.out.println("changed = (portNumber != i)" +changed +portNumber +"   "+ i);
        portNumber = i;
        content.set(listenKey, LISTEN + " " + portNumber);

    }

    public void setPortNumber(String i) {
        int val;
        try {
            val = Integer.parseInt(i.trim());
            setPortNumber(val);
        } catch (NumberFormatException e) {
            e.printStackTrace();
        }

    }

    public String getDocumentRoot() {
        return docRoot;
    }

    public void setDocumentRoot(String d) {
        if (!d.equals(initialDocRoot)) {
            docRoot = d;
            changed = true;
            System.out.println("    doc root changed..."+d+initialDocRoot);
            content.set(documentRootKey, DOCROOT + " \"" + d + "\"");
            if (directorydocumentRootKey!=-1){//we also need to change the corresponding <Directory Entry
                content.set(directorydocumentRootKey, DIRECTORY + " \"" + d + "\">");
                
            }

        }

    }
    public boolean isUserDirEnable(){
        return (userdirKey>0);  //userdir directive is in the file
        
    }
    public void setUserDirEnable(boolean state){
        if (initiaUserDirEnable==state){
            return;//do nothing: the state did not change from disk value'
        }
                
        if (state){
            if (userdirKey >0) {
                content.set(userdirKey,"UserDir public_html");
            } else {
                 content.add("UserDir public_html");

            }       
        }
        else {//remove the line if it is there
            if (userdirKey >0) {
                content.remove(userdirKey);
                userdirKey=0;
            }
            
        }
        changed=true;
    }
    public boolean isDirty() {
        return changed;
    }
}