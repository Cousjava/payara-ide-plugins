/*
 * Copyright (c) 1997-2011 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Sun Microsystems
 *     Oracle
 */


package com.sun.enterprise.jst.server.sunappsrv.commands;


import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URLDecoder;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.jar.Manifest;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Abstraction of commands for V3 server administration
 *
 * @author Peter Williams
 */
public abstract class ServerCommand {

    public static final char QUERY_SEPARATOR = '?'; // NOI18N
    public static final char PARAM_SEPARATOR = '&'; // NOI18N

    protected final String command;
    protected String query = null;
    protected boolean retry = false;
    private String serverMessage = "";

    public String getServerMessage() {
        return serverMessage;
    }

    public void  setServerMessage(String m) {
         serverMessage = m;
    }
    
    public ServerCommand(String command) {
        this.command = command;
    }
    
    /**
     * Returns server command represented by this object.  Set in constructor.
     * e.g. "deploy", "list-applications", etc.
     * 
     * @return command string represented by this object.
     */
    public String getCommand() {
        return command;
    }

    /**
     * Returns the query string for this command.  Set in constructor.
     * 
     * @return query string for this command.
     */
    public String getQuery() {
        return query;
    }

    /**
     * Override to change the type of HTTP method used for this command.
     * Default is GET.
     * 
     * @return HTTP method (GET, POST, etc.)
     */
    public String getRequestMethod() {
        return "GET"; // NOI18N
    }
    
    /**
     * Override and return true to send information to the server (HTTP POST).
     * Default is false.
     * 
     * @return true if using HTTP POST to send to server, false otherwise
     */
    public boolean getDoOutput() {
        return false;
    }
    
    /**
     * Override to set the content-type of information sent to the server.
     * Default is null (not set).
     * 
     * @return content-type of data sent to server via HTTP POST
     */
    public String getContentType() {
        return null;
    }
    
    /**
     * Override to provide a data stream for POST requests.  Data will be read
     * from this stream [until EOF?] and sent to the server.
     * 
     * @return a new InputStream derivative that provides the data to send
     *  to the server.  Caller is responsible for closing the stream.  Can
     *  return null, in which case no data will be sent.
     */
    public InputStream getInputStream() {
        return null;
    }

    /**
     * Override to provide a name for the data source whose inputstream is
     * returned by getInputStream.  Must not return null if getInputStream()
     * does not return null;
     *
     * @return the name to associate with the input stream
     */
    public String getInputName() {
        return null;
    }

    /**
     * Override to provide the lastModified date for data source whose
     * inputstream is returned by getInputStream.  Must not return null if
     * getInputStream() does not return null;
     *
     * @return String format of long integer from lastModified date of source.
     */
    public String getLastModified() {
        return null;
    }

    /**
     * Sometimes (e.g. during startup), the server does not accept commands.  In
     * such cases, it will block for 20 seconds and then return with the message
     * " V3 cannot process this command at this time, please wait".
     *
     * In such cases, we set a flag and have the option to reissue the command.
     *
     * @return true if server responded with it's "please wait" message.
     */
    public boolean retry() {
        return retry;
    }
    
    /**
     * Override for command specific failure checking.
     * 
     * @param responseCode code returned by http request
     * @return true if response was acceptable (e.g. 200) and handling of result
     * should proceed.
     */
    public boolean handleResponse(int responseCode) {
        return responseCode == 200;
    }
    
    /**
     * If the response for this command is in Manifest format (most or all
     * server commands use this), then override {@link #readManifest(Manifest)} 
     * instead.
     * <br>&nbsp;<br>
     * Override to read the response data sent by the server.  Do not close
     * the stream parameter when finished.  Caller will take care of that.
     * 
     * @param in Stream to read data from.
     * @return true if response was read correctly.
     * @throws java.io.IOException in case of stream error.
     */
    public boolean readResponse(InputStream in, HttpURLConnection hconn) throws IOException {
        boolean result = false;
        retry = false;

        Manifest m = new Manifest();
        m.read(in);
        String outputCode = m.getMainAttributes().getValue("exit-code"); // NOI18N
        if(outputCode.equalsIgnoreCase("Success")) { // NOI18N
            readManifest(m);
            result = true;
        } else {
            // !PW FIXME Need to pass this message back.  Need <Result> object?
            String message = m.getMainAttributes().getValue("message"); // NOI18N

            // If server is not currently available for processing commands,
            // set the retry flag.
            if(message != null && message.contains("please wait")) {
                retry = true;
            } else {
                serverMessage = null != message ? message.replaceAll("%%%EOL%%%", "\n") : "";
            }
            Logger.getLogger("glassfish").log(Level.WARNING, message);
        }

        return result;
    }
    
    /**
     * Override to interpret the manifest result returned from the server.
     * This method is only called if the manifest is successfully read and 
     * the exit-code field indicates the command was successful.
     * 
     * @param manifest Result returned by the server for this command in
     * manifest format.  The actual fields present depend on the command sent.
     * 
     * @throws java.io.IOException
     */
    public void readManifest(Manifest manifest) throws IOException {
    }
    
    /**
     * Override to parse, validate, and/or format any data read from the 
     * server in readResponse() / readManifest().
     * 
     * @return true if data was processed correctly.
     */
    public boolean processResponse() {
        return true;
    }
    
    /**
     * Command string for this command.
     * 
     * @return Command string for this command.
     */
    @Override
    public String toString() {
        return (query == null) ? command : command + QUERY_SEPARATOR + query;
    }

    public String getSrc() {
        return "/__asadmin/";
    }

    public boolean acceptsGzip() {
        return false;
    }

    /**
     * Command to get property information for a dotted name.
     */
    public static final class GetPropertyCommand extends ServerCommand {

        private Manifest info;
        private Map<String,String> propertyMap;

        public GetPropertyCommand(final String property) {
            super("get"); // NOI18N
            
            this.query = "pattern=" + property; // NOI18N
            this.propertyMap = new HashMap<String, String>();
        }

        @Override
        public void readManifest(Manifest manifest) throws IOException {
            info = manifest;
        }

        @Override
        public boolean processResponse() {
            if(info == null) {
                return false;
            }

            for (String key : info.getEntries().keySet()) {
                int equalsIndex = key.indexOf('=');
                if(equalsIndex >= 0) {
                    try {
                        propertyMap.put(key.substring(0, equalsIndex), URLDecoder.decode(URLDecoder.decode(key.substring(equalsIndex + 1), "UTF-8"),"UTF-8"));
                    } catch (UnsupportedEncodingException ex) {
                        ///Exceptions.printStackTrace(ex);
                    }
                } else {
                    propertyMap.put(key, "");
                }
            }

            return true;
        }

        public Map<String, String> getData() {
            return propertyMap;
        }
    }

    /**
     * Command to set the value of a dotted name property.
     */
    public static final class SetPropertyCommand extends ServerCommand {

        private Manifest info;

        public SetPropertyCommand(final String property, final String value, String format) {
            super("set"); // NOI18N
            query = MessageFormat.format(format, property,value); 
        }

        @Override
        public void readManifest(Manifest manifest) throws IOException {
            info = manifest;
        }

        @Override
        public boolean processResponse() {
            if(info == null) {
                return false;
            }

            return true;
        }
    }

}
