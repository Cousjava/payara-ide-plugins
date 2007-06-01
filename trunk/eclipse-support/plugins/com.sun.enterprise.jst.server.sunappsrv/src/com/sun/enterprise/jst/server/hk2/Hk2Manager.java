// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * glassfishplugins/www/license/CDDLv1.0.txt or
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable,
 * add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your
 * own identifying information: Portions Copyright [yyyy]
 * [name of copyright owner]
 */
// </editor-fold>

package com.sun.enterprise.jst.server.hk2;


import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.jar.Attributes;
import java.util.jar.Manifest;





/** Implementation of management task that provides info about progress
 * @author Ludovic Champenois
 */
public class Hk2Manager implements  Runnable {
    
    /** RequestProcessor processor that serializes management tasks. */
    private static ExecutorService rp =Executors.newSingleThreadExecutor();


    
    /** Command that is executed on running server. */
    private String command;
    
    /** Output of executed command (parsed for list commands). */
    private String outputMessage;
    /** code of executed command (SUCCESS or FAILURE). */
    private String outputCode;
    
    private List<String> tmidNames;
    private List<String> outputContainers;
    
    
    /** InputStream of application data. */
    private InputStream istream;
    

    
    /** TargetModuleID of module that is managed. */
    private String tmId;
    
    public Hk2Manager() {
       
    }
    
    
    
    
    
    public void initialDeploy(  File dir)  {
        try {
            //file is someting like /Users/ludo/WebApplication91/build/web
            String docBaseURI = URLEncoder.encode(dir.getAbsoluteFile().toURI().toASCIIString(),"UTF-8");
            String docBase = dir.getParentFile().getParentFile().getName();
            String ctxPath = docBase;///ctx.getAttributeValue ("path");
            this.tmId =  ctxPath; 
            
//            command = "deploy?path=" + dir.getAbsoluteFile()+"?name="+docBaseURI; // NOI18N
            command = "deploy?path=" + dir.getAbsoluteFile()+"?name="+docBase; // NOI18N
            
            System.out.println("deploy command="+command);
            rp.submit(this);
        } catch (UnsupportedEncodingException ex) {
            ex.printStackTrace();
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }
    
    public void reDeploy(String targetModuleID)  {
        try {
            
            this.tmId = targetModuleID;
            
            command = "redeploy?name=" +targetModuleID; // NOI18N
            
            
            System.out.println("redeploy command="+command);
//            String msg = NbBundle.getMessage(Hk2ManagerImpl.class, "MSG_DeploymentInProgress");
//            pes.fireHandleProgressEvent(null, new Status(ActionType.EXECUTE, cmdType, msg, StateType.RUNNING));
            rp.submit(this);
            
        } catch (RuntimeException e) {
//            String msg = NbBundle.getMessage(Hk2ManagerImpl.class, "MSG_DeployBrokenContextXml");
//            pes.fireHandleProgressEvent(null, new Status(ActionType.EXECUTE, cmdType, msg, StateType.FAILED));
        }
    }
    
    public void stopServer() {
        try {
            
            command = "stop-domain"; // NOI18N
            

            rp.submit(this);
            // } catch (java.io.IOException ioex) {
            //     pes.fireHandleProgressEvent (null, new Status (ActionType.EXECUTE, cmdType, ioex.getLocalizedMessage (), StateType.FAILED));
        } catch (RuntimeException e) {
        }
    }
    public  String[] getTargetModuleID(){
        
        command = "list-applications"; // NOI18N
        System.out.println("in getTargetModuleID imple.....");
 
        run();
        System.out.println("tmidNames" + tmidNames);
        if (tmidNames==null){
            return null;
        }
        String ret[] = new String[tmidNames.size()];
        for (int i=0;i< tmidNames.size();i++){
            ret[i]=tmidNames.get(i);
        }
        
        
        return ret;
    }
    
    public void undeploy(String tmId) {
        
        this.tmId = tmId;
        command = "undeploy?name="+tmId; // NOI18N

        rp.submit(this);
    }
    
    
    
    
    
    /**
     * Translates a context path string into <code>application/x-www-form-urlencoded</code> format.
     */
    private static String encodePath(String str) {
        try {
            StringTokenizer st = new StringTokenizer(str, "/"); // NOI18N
            if (!st.hasMoreTokens()) {
                return str;
            }
            StringBuilder result = new StringBuilder();
            while (st.hasMoreTokens()) {
                result.append("/").append(URLEncoder.encode(st.nextToken(), "UTF-8")); // NOI18N
            }
            return result.toString();
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e); // this should never happen
        }
    }
    
    
    
    

    

    
    private void analyseServerOutput(InputStream fis){
        Manifest m = new Manifest();
        try {
            m.read(fis);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        try {
            fis.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        System.out.println("m"+m);
        tmidNames = new ArrayList();
        outputContainers = new ArrayList();
        
        outputCode = m.getMainAttributes().getValue("exit-code");
        outputMessage = m.getMainAttributes().getValue("message");
        if(outputMessage==null){
            outputMessage="";
        }
        System.out.println("Exit code is " + outputCode);
        if (!outputCode.equalsIgnoreCase("Success")) {
            System.out.println("message-> " + outputMessage);
            return;
        }else {
 //           pes.fireHandleProgressEvent(tmId, new Status(ActionType.EXECUTE, cmdType, outputMessage, StateType.COMPLETED));
            
        }
        
        String containers = m.getMainAttributes().getValue("children");
        if (containers==null) {
            // no container currently started.
            return;
        }
        StringTokenizer token = new StringTokenizer(containers, ",");
        while (token.hasMoreTokens()) {
            String container = token.nextToken();
            System.out.println("Container : " + container);
            outputContainers.add(container);
            // get container attributes
            Attributes contAttr = m.getAttributes(container);
            String apps = contAttr.getValue("children");
            if (apps==null) {
                // no app currently deployed in this container
                continue;
            }
            StringTokenizer appsToken = new StringTokenizer(apps, ",");
            while (appsToken.hasMoreTokens()) {
                String app = appsToken.nextToken();
                //  tmidNames.add(app);
                Attributes appAttr = m.getAttributes(app);
                System.out.println("Module deployed " + appAttr.getValue("message"));
                tmidNames.add(appAttr.getValue("message"));
            }
        }
    }
    
    /** Executes one management task. */
    public synchronized void run() {
 //       pes.fireHandleProgressEvent(tmId, new Status(ActionType.EXECUTE, cmdType, command , StateType.RUNNING));
        
        outputMessage = "";

        
        int retries = 4;
        
        URLConnection conn = null;
        InputStreamReader reader = null;
        
        URL urlToConnectTo = null;
        
        boolean failed = false;
        String msg = "";
        while (retries >= 0) {
            retries = retries - 1;
            try {
                
                // Create a connection for this command
                String uri = "http://localhost:8080/";
                String withoutSpaces = (uri + command).replaceAll(" ", "%20");  //NOI18N
                urlToConnectTo = new URL(withoutSpaces);
                System.out.println("withoutSpaces  "+withoutSpaces);
                
                
                conn = urlToConnectTo.openConnection();
                HttpURLConnection hconn = (HttpURLConnection) conn;
                
                // Set up standard connection characteristics
                hconn.setAllowUserInteraction(false);
                hconn.setDoInput(true);
                hconn.setUseCaches(false);
                if (istream != null) {
                    hconn.setDoOutput(true);
                    hconn.setRequestMethod("PUT");   // NOI18N
                    hconn.setRequestProperty("Content-Type", "application/octet-stream");   // NOI18N
                } else {
                    hconn.setDoOutput(false);
                    hconn.setRequestMethod("GET"); // NOI18N
                }
                hconn.setRequestProperty("User-Agent", "hk2-agent"); // NOI18N
                // Set up an authorization header with our credentials
////                Hk2Properties tp = tm.getHk2Properties();
////                String input = tp.getUsername () + ":" + tp.getPassword ();
////                String auth = new String(Base64.encode(input.getBytes()));
////                hconn.setRequestProperty("Authorization", // NOI18N
////                                         "Basic " + auth); // NOI18N
                
                // Establish the connection with the server
                hconn.connect();
                int respCode = hconn.getResponseCode();
                if (respCode == HttpURLConnection.HTTP_UNAUTHORIZED
                        || respCode == HttpURLConnection.HTTP_FORBIDDEN) {
                    // connection to manager has not been allowed
                 //   authorized = false;
                    String errMsg = "MSG_AuthorizationFailed";
///                    pes.fireHandleProgressEvent(null, new Status(ActionType.EXECUTE, cmdType, errMsg, StateType.FAILED));
                    return;
                }
                if (Boolean.getBoolean("hk2.LogManagerCommands")) { // NOI18N
                    int code = hconn.getResponseCode();
                    String message = "  receiving response, code: " + code;
                    System.out.println(message);
                }
                // Send the request data (if any)
                if (istream != null) {
                    BufferedOutputStream ostream =
                            new BufferedOutputStream(hconn.getOutputStream(), 1024);
                    byte buffer[] = new byte[1024];
                    while (true) {
                        int n = istream.read(buffer);
                        if (n < 0) {
                            break;
                        }
                        ostream.write(buffer, 0, n);
                    }
                    ostream.flush();
                    ostream.close();
                    istream.close();
                }
                
                // Process the response message
                analyseServerOutput(hconn.getInputStream());
                return;

                
            } catch (Exception e) {
                if (retries < 0) {
  ////                  pes.fireHandleProgressEvent(tmId, new Status(ActionType.EXECUTE, cmdType, e.getLocalizedMessage(), StateType.FAILED));
                    failed = true;
                }
                // throw t;
            } finally {
                System.out.println("output is...:"+outputMessage);
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (java.io.IOException ioe) { // ignore this
                    }
                    reader = null;
                }
                if (istream != null) {
                    try {
                        istream.close();
                    } catch (java.io.IOException ioe) { // ignore this
                    }
                    istream = null;
                }
            }
            if (retries >=0) {
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {}
            }
        } // while
        if (!failed) {
///            pes.fireHandleProgressEvent(tmId, new Status(ActionType.EXECUTE, cmdType, msg, StateType.COMPLETED));
        }
    }
}
