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
public class HttpdConfModel extends Model {

    int listenKey = 0;
    int documentRootKey = 0;
    int portNumber = 80;
    String docRoot = "";
    
    /* value read from the file, before possible user modification
     * */
    
    String initialDocRoot;
    boolean changed = false;
    static private String LISTEN = "Listen";// port key
    static private String DOCROOT = "DocumentRoot";// root key
    public HttpdConfModel() {
        super(new File(Environment.getHttpdconf()));
    }

    @Override
    public void reset() {
        initialDocRoot="";
        documentRootKey=-1;
        listenKey=-1;
        
         changed = false;
        load();
    }

    @Override
    public void lineAddedCallBack(String line, int lineNumber) {
        if (line.startsWith(LISTEN)) {
            listenKey = lineNumber;
            System.out.println("Line is " + line);

            try {
                portNumber = Integer.parseInt((line.substring(LISTEN.length(), line.length())).trim());
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        if (line.startsWith(DOCROOT)) {
            documentRootKey = lineNumber;
            docRoot = line.substring(DOCROOT.length(), line.length()).trim();
            if (docRoot.startsWith("\"")) {
                docRoot = docRoot.substring(1);
            }
            if (docRoot.endsWith("\"")) {
                docRoot = docRoot.substring(0, docRoot.length() - 1);
                initialDocRoot = docRoot;
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
            content.set(documentRootKey, DOCROOT + " \"" + d + "\"");

        }

    }

    public boolean isDirty() {
        return changed;
    }
}