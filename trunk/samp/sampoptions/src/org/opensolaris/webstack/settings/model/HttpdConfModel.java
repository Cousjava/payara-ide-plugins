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
    boolean changed = false;
    static private String LISTEN = "Listen";// port key
    public HttpdConfModel() {
        super(new File(Environment.getHttpdconf()));
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
        if (line.startsWith("DocumentRoot")) {
            documentRootKey = lineNumber;
        }
    }

    public int getPortNumber() {
        return portNumber;


    }

    public void setPortNumber(int i) {
        System.out.println("change port number is "+changed);
        System.out.println("change port number 0is "+(portNumber != i));
        System.out.println("change port number portNumber "+(portNumber ));
        System.out.println("change port number i  "+(i ));
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

    public boolean isDirty() {
        return changed;
    }
}