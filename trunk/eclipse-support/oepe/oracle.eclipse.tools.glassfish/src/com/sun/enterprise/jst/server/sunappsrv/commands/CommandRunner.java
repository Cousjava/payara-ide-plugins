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


import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
//import java.net.Authenticator;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.eclipse.osgi.internal.signedcontent.Base64;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;
import com.sun.enterprise.jst.server.sunappsrv.commands.GlassfishModule.OperationState;
import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand.GetPropertyCommand;
import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand.SetPropertyCommand;



/** 
 * Implementation of management task that provides info about progress
 * NOTE: This is a copy of the file used in the NB V3 plugin, though it's 
 * in a different package there and the Base64 library is used here and the 
 * retry logic in the call() method is slightly different here.
 *
 * @author Peter Williams
 */
@SuppressWarnings("restriction")
public class CommandRunner extends BasicTask<OperationState> {
    
    public final int HTTP_RETRY_DELAY = 3000;
    
    /** Executor that serializes management tasks. 
     */
    private static ExecutorService executor;
    
//    private static Authenticator AUTH = new AdminAuthenticator();
   
    /** Returns shared executor.
     */
    private static synchronized ExecutorService executor() {
        if(executor == null) {
            executor = Executors.newFixedThreadPool(1);
        }
        return executor;
    }
    
    /** Command type used for events. */
    private ServerCommand serverCmd;
    
    /** Has been the last access to  manager web app authorized? */
    private boolean authorized;
    private final CommandFactory cf;
    private final boolean isReallyRunning;
    private SunAppServer server;
    
    public CommandRunner(SunAppServer server) {
        super(null);
        this.cf = new CommandFactory()  {
            //@Override
            public SetPropertyCommand getSetPropertyCommand(String name, String value) {
                return new ServerCommand.SetPropertyCommand(name, value,
                        "DEFAULT={0}={1}"); // NOI18N
            }
            
        };
    	this.server =server;
        this.isReallyRunning = true;
    }
    
    public CommandRunner(SunAppServer server, boolean isReallyRunning, CommandFactory cf, Map<String, String> properties, OperationStateListener... stateListener) {
        super(properties, stateListener);
        this.cf =cf;
    	this.server =server;
        this.isReallyRunning = isReallyRunning;
    }    
    /**
     * Sends stop-domain command to server (asynchronous)
     * 
     */
    public Future<OperationState> stopServer() {
        return execute(Commands.STOP, "MSG_STOP_SERVER_IN_PROGRESS");
        
    }
    
     /**
     * Sends restart-domain command to server (asynchronous)
     *
     */
    public Future<OperationState> restartServer(int debugPort, String query) {
        final String restartQuery = query; // cf.getRestartQuery(debugPort);
        if (-1 == debugPort || "".equals(restartQuery) ) {
            return execute(new ServerCommand("restart-domain") {

                @Override
                public String getQuery() {
                    return restartQuery;
                }
            }, "MSG_RESTART_SERVER_IN_PROGRESS"); // NOI18N
        }
        // force the options to be correct for remote debugging, then restart...
        CommandRunner inner = new CommandRunner(server, isReallyRunning, cf, ip, new OperationStateListener() {

            //@Override
            public void operationStateChanged(OperationState newState, String message) {
                //throw new UnsupportedOperationException("Not supported yet.");
            }

        });

        // I wish that the server folks had let me add a port number to the
        // restart-domain --debug command... but this will have to do until then

        ServerCommand.GetPropertyCommand getCmd = new ServerCommand.GetPropertyCommand("configs.config.server-config.java-config.debug-options");

        OperationState state = null;
        try {
            state = inner.execute(getCmd).get();
        } catch (InterruptedException ie) {
            Logger.getLogger("glassfish").log(Level.INFO,debugPort+"",ie);
        } catch (ExecutionException ee) {
            Logger.getLogger("glassfish").log(Level.INFO,debugPort+"",ee);
        }
        String qs = null;
        if (state == OperationState.COMPLETED) {
            Map<String, String> data = getCmd.getData();
            if (!data.isEmpty()) {
                // now I can reset the debug data
                String oldValue = data.get("configs.config.server-config.java-config.debug-options");
                ServerCommand.SetPropertyCommand setCmd =
                        cf.getSetPropertyCommand("configs.config.server-config.java-config.debug-options",
                        oldValue.replace("transport=dt_shmem", "transport=dt_socket").
                        replace("address=[^,]+", "address=" + debugPort));
                //serverCmd = setCmd;
                //task = executor.submit(this);
                try {
                    state = inner.execute(setCmd).get();
                    qs = "debug=true";
                } catch (InterruptedException ie) {
                     Logger.getLogger("glassfish").log(Level.INFO,debugPort+"",ie);
                } catch (ExecutionException ee) {
                     Logger.getLogger("glassfish").log(Level.INFO,debugPort+"",ee);
                }
            }
        }
        if (null == qs) {
            qs = "debug=false";
        }
        final String fqs = qs;
        return execute(new ServerCommand("restart-domain") {

            @Override
            public String getQuery() {
                return fqs;
            }
        }, "MSG_RESTART_SERVER_IN_PROGRESS");
    }
    
    /**
     * Sends list-applications command to server (synchronous)
     * 
     * @return String array of names of deployed applications.
     */
    public Map<String, List<AppDesc>> getApplications(String container) {
        Map<String, List<AppDesc>> result = Collections.emptyMap();
        try {
            Map<String, List<String>> apps = Collections.emptyMap();
            Commands.ListComponentsCommand cmd = new Commands.ListComponentsCommand(container);
            serverCmd = cmd;
            Future<OperationState> task = executor().submit(this);
            OperationState state = task.get();
            if (state == OperationState.COMPLETED) {
                apps = cmd.getApplicationMap();
            }
            ServerCommand.GetPropertyCommand getCmd = new ServerCommand.GetPropertyCommand("applications.application.*");
            serverCmd = getCmd;
            task = executor().submit(this);
            state = task.get();
            if (state == OperationState.COMPLETED) {
                result = processApplications(apps, getCmd.getData());
            }
        } catch (InterruptedException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
        } catch (ExecutionException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
        }
        return result;
    }
    
    private Map<String, List<AppDesc>> processApplications(Map<String, List<String>> appsList, Map<String, String> properties){
        Map<String, List<AppDesc>> result = new HashMap<String, List<AppDesc>>();
        Iterator<String> appsItr = appsList.keySet().iterator();
        while (appsItr.hasNext()) {
            String engine = appsItr.next();
            List<String> apps = appsList.get(engine);
            for (int i = 0; i < apps.size(); i++) {
                String name = apps.get(i).trim();
                String appname = "applications.application." + name; // NOI18N
                String contextKey = appname + ".context-root"; // NOI18N
                String pathKey = appname + ".location"; // NOI18N

                String contextRoot = properties.get(contextKey);
                if (contextRoot == null) {
                    contextRoot = name;
                }
                if (contextRoot.startsWith("/")) {  // NOI18N
                    contextRoot = contextRoot.substring(1);
                }

                String path = properties.get(pathKey);
                if (path == null) {
                    path = "unknown"; //NOI18N
                }
                if (path.startsWith("file:")) {  // NOI18N
                    path = path.substring(5);
                }

                List<AppDesc> appList = result.get(engine);
                if(appList == null) {
                    appList = new ArrayList<AppDesc>();
                    result.put(engine, appList);
                }
                appList.add(new AppDesc(name, path, contextRoot));
            }
        }
        return result;
    }
  
       /**
     * Sends list-web-services command to server (synchronous)
     *
     * @return String array of names of deployed applications.
     */
    public List<WSDesc> getWebServices() {
        List<WSDesc> result = Collections.emptyList();
        try {
            List<String> wss = Collections.emptyList();
            Commands.ListWebservicesCommand cmd = new Commands.ListWebservicesCommand();
            serverCmd = cmd;
            Future<OperationState> task = executor().submit(this);
            OperationState state = task.get();
            if (state == OperationState.COMPLETED) {
                wss = cmd.getWebserviceList();

                result = processWebServices(wss);
            }
        } catch (InterruptedException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
        } catch (ExecutionException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
        }
        return result;
    }

    private List<WSDesc> processWebServices(List<String> wssList){
        List<WSDesc> result = new  ArrayList<WSDesc>();
        for (String a : wssList) {
            result.add(new WSDesc(a, a+"?wsdl", a+"?Tester")); // NOI18N
        }
        return result;
    }

    public List<ResourceDesc> getResources(String type) {
        List<ResourceDesc> result = Collections.emptyList();
        try {
            Commands.ListResourcesCommand cmd = new Commands.ListResourcesCommand(type);
            serverCmd = cmd;
            Future<OperationState> task = executor().submit(this);
            OperationState state = task.get();
            if (state == OperationState.COMPLETED) {
                result = cmd.getResourceList();
            }
        } catch (InterruptedException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);
        } catch (ExecutionException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);
        }
        return result;
    }
    
    public Map<String, String> getResourceData(String name) {
        try {
            GetPropertyCommand cmd;
            String query;
            // see https:/glassfish.dev.java.net/issues/show_bug.cgi?id=7296
            // revert this, when the server side of issue is resolved
            //if (null != name) {
            //    query = "resources.*."+name+".*"; //$NON-NLS-1$ //$NON-NLS-2$
            //} else {
                query = "resources.*"; //$NON-NLS-1$
            //}
            cmd = new ServerCommand.GetPropertyCommand(query); 
            serverCmd = cmd;
            Future<OperationState> task = executor().submit(this);
            OperationState state = task.get();
            if (state == OperationState.COMPLETED) {
                Map<String,String> retVal = cmd.getData();
                if (retVal.isEmpty())
                    Logger.getLogger("glassfish").log(Level.INFO, null, new IllegalStateException(query+" has no data"));  //$NON-NLS-1$
                return retVal;
            }
        } catch (InterruptedException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  //$NON-NLS-1$
        } catch (ExecutionException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  //$NON-NLS-1$
        }
        return new HashMap<String,String>();
    }
    
    public void putResourceData(Map<String, String> data) throws PartialCompletionException {
        Set<String> keys = data.keySet();
        String itemsNotUpdated = null;
        Throwable lastEx = null;
        for (String k : keys) {
            String compName = k;
            String compValue = data.get(k);

            try {
                SetPropertyCommand cmd = server.getCommandFactory().getSetPropertyCommand(compName, compValue);
                serverCmd = cmd;
                Future<OperationState> task = executor().submit(this);
                OperationState state = task.get();
                if (state == OperationState.COMPLETED) {
                    cmd.processResponse();
                //return cmd.getData();
                }
            } catch (InterruptedException ex) {
                lastEx = ex;
                Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
                itemsNotUpdated = addName(compName, itemsNotUpdated);
            } catch (ExecutionException ex) {
                lastEx = ex;
                Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
                itemsNotUpdated = addName(compName, itemsNotUpdated);
            }
        //return new HashMap<String,String>();
        }
        if (null != itemsNotUpdated) {
            PartialCompletionException pce = new PartialCompletionException(itemsNotUpdated);
            if (null != lastEx) {
                pce.initCause(lastEx);
            }
            throw pce;
        }
    }

    public Future<OperationState> deploy(File dir) {
        return deploy(dir, dir.getParentFile().getName(), null);
    }

    public Future<OperationState> deploy(File dir, String moduleName) {
        return deploy(dir, moduleName, null);
    }
    
    public Future<OperationState> deploy(File dir, String moduleName, String contextRoot)  {
        return deploy(dir, moduleName, contextRoot, null, new File[0]);
    }
    
    public Future<OperationState> deploy(File dir, String moduleName, String contextRoot, Map<String,String> properties, File[] libraries) {
       // LogViewMgr.displayOutput(ip,null);
        return execute(new Commands.DeployCommand(dir, moduleName,
                contextRoot, computePreserveSessions(), properties, libraries));
    }

    public Future<OperationState> redeploy(String moduleName, String contextRoot, File[] libraries, boolean resourcesChanged)  {
        //LogViewMgr.displayOutput(ip,null);
        return execute(new Commands.RedeployCommand(moduleName, contextRoot, 
                computePreserveSessions(), libraries, resourcesChanged));
    }

    private  Boolean computePreserveSessions() {
        return server.getKeepSessions().equals("true");
    }
    
    public Future<OperationState> undeploy(String moduleName) {
        return execute(new Commands.UndeployCommand(moduleName));
    }
    
     public Future<OperationState> enable(String moduleName) {
        return execute(new Commands.EnableCommand(moduleName));
    }

    public Future<OperationState> disable(String moduleName) {
        return execute(new Commands.DisableCommand(moduleName));
    }
    
    public Future<OperationState> unregister(String resourceName, String suffix, String cmdPropName, boolean cascade) {
        return execute(new Commands.UnregisterCommand(resourceName, suffix, cmdPropName, cascade));
    }

    /**
     * Execute an abitrary server command.
     */
    public Future<OperationState> execute(ServerCommand command) {
        return execute(command, null);
    }

    private String addName(final String compName, final String itemsNotUpdated) {
        String retVal = itemsNotUpdated;
        if (null != itemsNotUpdated) {
            retVal += ", "+compName;
        } else {
            retVal = compName;
        }
        return retVal;
    }
    
    private Future<OperationState> execute(ServerCommand command, String msgResId) {
        serverCmd = command;
        if(msgResId != null) {
            fireOperationStateChanged(OperationState.RUNNING, msgResId, instanceName);
        }
        return executor().submit(this);
    }
    
    /** Executes one management task. 
     */
    @Override
    public OperationState call() {
        fireOperationStateChanged(OperationState.RUNNING, "MSG_ServerCmdRunning", 
                serverCmd.toString(), instanceName);
        
        boolean httpSucceeded = false;
        boolean commandSucceeded = false;
        URL urlToConnectTo = null;
        URLConnection conn = null;
        String commandUrl;
        
        try {
            commandUrl = constructCommandUrl(serverCmd.getSrc(), serverCmd.getCommand(), serverCmd.getQuery());
        } catch (URISyntaxException ex) {
            return fireOperationStateChanged(OperationState.FAILED, "MSG_ServerCmdException",  // NOI18N
                    serverCmd.toString(), instanceName, ex.getLocalizedMessage());
        }

        int retries = 1; // disable ("version".equals(cmd) || "__locations".equals(cmd)) ? 1 : 3;
        Logger.getLogger("glassfish").log(Level.FINEST, "CommandRunner.call({0}) called on thread \"{1}\"", new Object[]{commandUrl, Thread.currentThread().getName()}); // NOI18N

        // Create a connection for this command
        try {
            urlToConnectTo = new URL(commandUrl);

            while ((!httpSucceeded && retries-- > 0) || serverCmd.retry() ) {
                try {
                    Logger.getLogger("glassfish").log(Level.FINE, "HTTP Command: {0}", commandUrl); // NOI18N

                    conn = urlToConnectTo.openConnection();
                    if(conn instanceof HttpURLConnection) {
                        HttpURLConnection hconn = (HttpURLConnection) conn;

                        if (conn instanceof HttpsURLConnection) {
                            // let's just trust any server that we connect to...
                            // we aren't send them money or secrets...
                            TrustManager[] tm = new TrustManager[]{
                                new X509TrustManager() {

                                    public void checkClientTrusted(X509Certificate[] arg0, String arg1) throws CertificateException {
                                        return;
                                    }

                                    public void checkServerTrusted(X509Certificate[] arg0, String arg1) throws CertificateException {
                                        return;
                                    }

                                    public X509Certificate[] getAcceptedIssuers() {
                                        return null;
                                    }
                                }
                            };

                            SSLContext context = null;
                            try {
                                context = SSLContext.getInstance("SSL");
                                context.init(null, tm, null);
                                ((HttpsURLConnection)hconn).setSSLSocketFactory(context.getSocketFactory());
                                ((HttpsURLConnection)hconn).setHostnameVerifier(new HostnameVerifier() {

                                    public boolean verify(String string, SSLSession ssls) {
                                        return true;
                                    }

                                });
                            } catch (Exception ex) {
                                // if there is an issue here... there will be another exception later
                                // which will take care of the user interaction...
                                Logger.getLogger("glassfish").log(Level.INFO, "trust manager problem: " + urlToConnectTo,ex); // NOI18N
                            }

                        }

                        // Set up standard connection characteristics
                        hconn.setReadTimeout(300000); //ie 300 secs see bug 328
                        hconn.setAllowUserInteraction(false);
                        hconn.setDoInput(true);
                        hconn.setUseCaches(false);
                        hconn.setRequestMethod(serverCmd.getRequestMethod());
                        hconn.setDoOutput(serverCmd.getDoOutput());
                        String contentType = serverCmd.getContentType();
                        if(contentType != null && contentType.length() > 0) {
                            hconn.setRequestProperty("Content-Type", contentType); // NOI18N
                            hconn.setChunkedStreamingMode(0);
                        }
                        hconn.setRequestProperty("User-Agent", "hk2-agent"); // NOI18N
                        if (serverCmd.acceptsGzip()) {
                            hconn.setRequestProperty("Accept-Encoding", "gzip");
                        }

                        
                        if ( !server.getUseAnonymousConnections().equals("true")){
                        	// Set up an authorization header with our credentials
                        	// Hk2Properties tp = tm.getHk2Properties();
                        	String input = server.getAdminName() + ":" + server.getAdminPassword();
                        	String auth = new String(Base64.encode(input.getBytes()));
                        	hconn.setRequestProperty("Authorization", // NOI18N
                        			"Basic " + auth); // NOI18N
                        }

                        // Establish the connection with the server
                       /// Authenticator.setDefault(AUTH);
                        hconn.connect();
                        // Send data to server if necessary
                        handleSend(hconn);

                        int respCode = hconn.getResponseCode();
                        if(respCode == HttpURLConnection.HTTP_UNAUTHORIZED || 
                                respCode == HttpURLConnection.HTTP_FORBIDDEN) {
                            // connection to manager has not been allowed
                            authorized = false;
                            if(respCode == HttpURLConnection.HTTP_UNAUTHORIZED)
                            	serverCmd.setServerMessage("Remote access is not authorized. Invalid user name or password.");
                            if(respCode == HttpURLConnection.HTTP_FORBIDDEN)
                            	serverCmd.setServerMessage("Remote access is forbidden. Remote Server should be secured. (via enable-secure-admin command).");
                            return fireOperationStateChanged(OperationState.FAILED, 
                                    "MSG_AuthorizationFailed", serverCmd.toString(), instanceName); // NOI18N
                        }

                        // !PW FIXME log status for debugging purposes
                        if(Boolean.getBoolean("org.netbeans.modules.hk2.LogManagerCommands")) { // NOI18N
                            Logger.getLogger("glassfish").log(Level.FINE, "  receiving response, code: {0}", respCode); // NOI18N
                        }

                        // Process the response message
                        if(handleReceive(hconn)) {
                            commandSucceeded = serverCmd.processResponse();
                        }
                        
                        httpSucceeded = true;
                    } else {
                        Logger.getLogger("glassfish").log(Level.INFO, "Unexpected connection type: {0}", urlToConnectTo); // NOI18N
                    }
                } catch(ProtocolException ex) {
                    fireOperationStateChanged(OperationState.FAILED, "MSG_Exception", // NOI18N
                            ex.getLocalizedMessage());
                    retries = 0;
                } catch(IOException ex) {
                    if(retries <= 0) {
                        fireOperationStateChanged(OperationState.FAILED, "MSG_Exception", // NOI18N
                                ex.getLocalizedMessage());
                    }
                }
                
                if(!httpSucceeded && retries > 0) {
                    try {
                        Thread.sleep(HTTP_RETRY_DELAY);
                    } catch (InterruptedException e) {}
                }
            } // while
        } catch(MalformedURLException ex) {
            Logger.getLogger("glassfish").log(Level.WARNING, ex.getLocalizedMessage(), ex); // NOI18N
        }
        
        if(commandSucceeded) {
            return fireOperationStateChanged(OperationState.COMPLETED, "MSG_ServerCmdCompleted", // NOI18N
                    serverCmd.toString(), instanceName);
        } else {
            return fireOperationStateChanged(OperationState.FAILED, "MSG_ServerCmdFailed", // NOI18N
                    serverCmd.toString(), instanceName, serverCmd.getServerMessage());
        }
    }
    
     private String constructCommandUrl(final String cmdSrc, final String cmd, final String query) throws URISyntaxException {
         String host = server.getServer().getHost();
         int port = Integer.parseInt(server.getAdminServerPort());

         URI uri = new URI(Utils.getHttpListenerProtocol(host,port), null, host, port, cmdSrc + cmd, query, null); // NOI18N
         return uri.toASCIIString().replace("+", "%2b"); // these characters don't get handled by GF correctly... best I can tell.
     }    

    /*
     * Note: this is based on reading the code of CLIRemoteCommand.java
     * from the server's code repository... Since some asadmin commands
     * need to send multiple files, the server assumes the input is a ZIP
     * stream.
     */
    private void handleSend(HttpURLConnection hconn) throws IOException {
        InputStream istream = serverCmd.getInputStream();
        if(istream != null) {
            ZipOutputStream ostream = null;
            try {
                ostream = new ZipOutputStream(new BufferedOutputStream(hconn.getOutputStream(), 1024*1024));
                ZipEntry e = new ZipEntry(serverCmd.getInputName());
                e.setExtra(getExtraProperties());
                ostream.putNextEntry(e);
                byte buffer[] = new byte[1024*1024];
                while (true) {
                    int n = istream.read(buffer);
                    if (n < 0) {
                        break;
                    }
                    ostream.write(buffer, 0, n);
                }
                ostream.closeEntry();
                ostream.flush();
            } finally {
                try {
                    istream.close();
                } catch(IOException ex) {
                    Logger.getLogger("glassfish").log(Level.INFO, ex.getLocalizedMessage(), ex); // NOI18N
                }
                if(ostream != null) {
                    try { 
                        ostream.close(); 
                    } catch(IOException ex) {
                        Logger.getLogger("glassfish").log(Level.INFO, ex.getLocalizedMessage(), ex);  // NOI18N
                    }
                    ostream = null;
                }
            }
        } else if("POST".equalsIgnoreCase(serverCmd.getRequestMethod())) { // NOI18N
            Logger.getLogger("glassfish").log(Level.INFO, "HTTP POST request but no data stream provided"); // NOI18N
        }
    }
    
    private byte[] getExtraProperties() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Properties props = new Properties();
        props.setProperty("data-request-type", "file-xfer"); // NOI18N
        props.setProperty("last-modified", serverCmd.getLastModified()); // NOI18N
        props.put("data-request-name", "DEFAULT");
        props.put("data-request-is-recursive", "true");
        props.put("Content-Type", "application/octet-stream");
        props.list(new java.io.PrintStream(baos));
        return baos.toByteArray();
    }

    private boolean handleReceive(HttpURLConnection hconn) throws IOException {
        boolean result = false;
        InputStream httpInputStream = hconn.getInputStream();
        try {
            result = serverCmd.readResponse(httpInputStream, hconn);
        } finally {
            try {
                httpInputStream.close();
            } catch (IOException ex) {
                Logger.getLogger("glassfish").log(Level.INFO, ex.getLocalizedMessage(), ex);  // NOI18N
            }
        }
        return result;
    }

}
