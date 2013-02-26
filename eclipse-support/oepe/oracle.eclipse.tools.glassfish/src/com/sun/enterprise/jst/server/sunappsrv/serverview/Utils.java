/*
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
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


package com.sun.enterprise.jst.server.sunappsrv.serverview;

import java.io.IOException;
import java.net.ConnectException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Nitya Doraisamy
 */
public class Utils {
    public static String simplifyModuleID(String candidateID) {
        String moduleID = null;

        moduleID = candidateID.replace(' ', '_');
        if (moduleID.startsWith("/")) { //$NON-NLS-1$
            moduleID = moduleID.substring(1);
        }

        // This moduleID will be later used to construct file path,
        // replace the illegal characters in file name
        //  \ / : * ? " < > | with _
        moduleID = moduleID.replace('\\', '_').replace('/', '_');
        moduleID = moduleID.replace(':', '_').replace('*', '_');
        moduleID = moduleID.replace('?', '_').replace('"', '_');
        moduleID = moduleID.replace('<', '_').replace('>', '_');
        moduleID = moduleID.replace('|', '_');

        // This moduleID will also be used to construct an ObjectName
        // to register the module, so replace additional special
        // characters , =  used in property parsing with -
        moduleID = moduleID.replace(',', '_').replace('=', '_');

        return moduleID;
    }
    /**
     * Determine if a local port is occupied.
     *
     * @param port
     * @return true, if the local port is in use.
     */
    public static boolean isLocalPortOccupied(int port) {
        ServerSocket ss = null;
        boolean retVal = true;
        try {
            ss = new ServerSocket(port);
            retVal = false;
        } catch (IOException ioe) {
            // do nothing
        } finally {
            if (null != ss) {try { ss.close(); } catch (IOException ioe) {} }
        }
        return retVal;
    }
    
    /**
     * identify the http/https protocol designator for a port
     *
     */
    public static String getHttpListenerProtocol(String hostname, String port) {
        String retVal = "http";
        try {
            retVal = getHttpListenerProtocol(hostname, Integer.parseInt(port));
        } catch (NumberFormatException nfe) {
            Logger.getLogger("glassfish").log(Level.INFO, "returning http due to exception", nfe);
        }
        return retVal;
    }

    /**
     * identify the http/https protocol designator for a port
     *
     */
    public static String getHttpListenerProtocol(String hostname, int port) {
        String retVal = "http";
        try {
            if (isSecurePort(hostname, port)) {
                retVal = "https";
            }
        } catch (ConnectException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, null, ex);
        } catch (SocketException ex) {
            Logger.getLogger("glassfish").log(Level.FINE, null, ex);
        } catch (SocketTimeoutException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, null, ex);
        } catch (IOException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, null, ex);
        }
        return retVal;
    }

    private static final int PORT_CHECK_TIMEOUT = 4000; // Port check timeout in ms

    /**
     * Determine whether an http listener is secure or not..
     *
     *  This method accepts a hostname and port #.  It uses this information
     *  to attempt to connect to the port, send a test query, analyze the
     *  result to determine if the port is secure or unsecure (currently only
     *  http / https is supported).
     * it might emit a warning in the server log for GlassFish cases
     * No Harm, just an annoying warning, so we need to use this call only when really needed
     *
     * @param hostname the host for the http-listener
     * @param port the port for the http-listener
     * @throws IOException
     * @throws SocketTimeoutException
     * @throws ConnectException
     */
    public static boolean isSecurePort(String hostname, int port)
            throws IOException, ConnectException, SocketTimeoutException {
        return isSecurePort(hostname,port, 0);
    }

    private static boolean isSecurePort(String hostname, int port, int depth)
            throws IOException, ConnectException, SocketTimeoutException {
        // Open the socket with a short timeout for connects and reads.
        Socket socket = new Socket();
        try {
            socket.connect(new InetSocketAddress(hostname, port), PORT_CHECK_TIMEOUT);
            socket.setSoTimeout(PORT_CHECK_TIMEOUT);
        } catch(SocketException ex) { // this could be bug 70020 due to SOCKs proxy not having localhost
            String socksNonProxyHosts = System.getProperty("socksNonProxyHosts");
            if(socksNonProxyHosts != null && socksNonProxyHosts.indexOf("localhost") < 0) {
                String localhost = socksNonProxyHosts.length() > 0 ? "|localhost" : "localhost";
                System.setProperty("socksNonProxyHosts",  socksNonProxyHosts + localhost);
                if (depth < 1) {
                    socket.close();
                   return isSecurePort(hostname,port,1);
                } else {
                socket.close();
                ConnectException ce = new ConnectException();
                ce.initCause(ex);
                throw ce; //status unknow at this point
                //next call, we'll be ok and it will really detect if we are secure or not
                }
            }
        }

        // Send an https query (w/ trailing http query)
        java.io.OutputStream ostream = socket.getOutputStream();
        ostream.write(TEST_QUERY);

        // Get the result
        java.io.InputStream istream = socket.getInputStream();
        byte[] input = new byte[8192];
        istream.read(input);


        // Determine protocol from result
        // Can't read https response w/ OpenSSL (or equiv), so use as
        // default & try to detect an http response.
        String response = new String(input).toLowerCase(Locale.ENGLISH);
        boolean isSecure = true;
        if (response.length() == 0) {
            //isSecure = false;
            // Close the socket
            socket.close();
            throw new ConnectException();
        } else if (response.startsWith("http/1.1 302 moved temporarily")) {
            // 3.1 has started to use redirects... but 3.0 is still using the older strategies...
            isSecure = true;
        } else if (response.startsWith("http/1.")) {
            isSecure = false;
        } else if (response.indexOf("<html") != -1) {
            isSecure = false;
        } else if (response.indexOf("</html") != -1) {
            // New test added to resolve 106245
            // when the user has the IDE use a proxy (like webcache.foo.bar.com),
            // the response comes back as "d><title>....</html>".  It looks like
            // something eats the "<html><hea" off the front of the data that
            // gets returned.
            //
            // This test makes an allowance for that behavior. I figure testing
            // the likely "last bit" is better than testing a bit that is close
            // to the data that seems to get eaten.
            //
            isSecure = false;
        } else if (response.indexOf("connection: ") != -1) {
            isSecure = false;
        }
            // Close the socket
            socket.close();
            return isSecure;
    }

    /**
     *  This is the test query used to ping the server in an attempt to
     *  determine if it is secure or not.
     */
    private static byte [] TEST_QUERY = new byte [] {
        // The following SSL query is from nmap (http://www.insecure.org)
        // This HTTPS request should work for most (all?) https servers
        (byte)0x16, (byte)0x03, (byte)0x00, (byte)0x00, (byte) 'S', (byte)0x01,
        (byte)0x00, (byte)0x00, (byte) 'O', (byte)0x03, (byte)0x00, (byte) '?',
        (byte) 'G', (byte)0xd7, (byte)0xf7, (byte)0xba, (byte) ',', (byte)0xee,
        (byte)0xea, (byte)0xb2, (byte) '`', (byte) '~', (byte)0xf3, (byte)0x00,
        (byte)0xfd, (byte)0x82, (byte) '{', (byte)0xb9, (byte)0xd5, (byte)0x96,
        (byte)0xc8, (byte) 'w', (byte)0x9b, (byte)0xe6, (byte)0xc4, (byte)0xdb,
        (byte) '<', (byte) '=', (byte)0xdb, (byte) 'o', (byte)0xef, (byte)0x10,
        (byte) 'n', (byte)0x00, (byte)0x00, (byte) '(', (byte)0x00, (byte)0x16,
        (byte)0x00, (byte)0x13, (byte)0x00, (byte)0x0a, (byte)0x00, (byte) 'f',
        (byte)0x00, (byte)0x05, (byte)0x00, (byte)0x04, (byte)0x00, (byte) 'e',
        (byte)0x00, (byte) 'd', (byte)0x00, (byte) 'c', (byte)0x00, (byte) 'b',
        (byte)0x00, (byte) 'a', (byte)0x00, (byte) '`', (byte)0x00, (byte)0x15,
        (byte)0x00, (byte)0x12, (byte)0x00, (byte)0x09, (byte)0x00, (byte)0x14,
        (byte)0x00, (byte)0x11, (byte)0x00, (byte)0x08, (byte)0x00, (byte)0x06,
        (byte)0x00, (byte)0x03, (byte)0x01, (byte)0x00,
        // The following is a HTTP request, some HTTP servers won't
        // respond unless the following is also sent
        (byte) 'G', (byte) 'E', (byte) 'T', (byte) ' ', (byte) '/',
        // change the detector to request something that the monitor knows to filter
        //  out.  This will work-around 109891. Use the longest filtered prefix to
        //  avoid false positives....
        (byte) 'c', (byte) 'o', (byte) 'm', (byte) '_', (byte) 's', (byte) 'u',
        (byte) 'n', (byte) '_', (byte) 'w', (byte) 'e', (byte) 'b', (byte) '_',
        (byte) 'u', (byte) 'i',
        (byte) ' ',
        (byte) 'H', (byte) 'T', (byte) 'T', (byte) 'P', (byte) '/', (byte) '1',
        (byte) '.', (byte) '0', (byte)'\n', (byte)'\n'
    };

}
