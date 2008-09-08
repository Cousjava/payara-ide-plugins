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

package com.sun.enterprise.jst.server.sunappsrv;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jst.server.generic.core.internal.GenericServerBehaviour;
import org.eclipse.jst.server.generic.core.internal.PingThread;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.IServer;

/**
 *
 *
 */
public class SunAppServerBehaviour extends GenericServerBehaviour {
    private static final String DEFAULT_DOMAIN_DIR_NAME = "domains"; //$NON-NLS-N$
    private static final String DEFAULT_DOMAIN_NAME = "domain1"; //$NON-NLS-N$
    
    /** Creates a new instance of SunAppServerBehaviour */
    public SunAppServerBehaviour() {
 //       SunAppSrvPlugin.logMessage("in SunAppServerBehaviour CTOR ");
      
    }



    /* get the correct adapter for  the GlassFish server
     * 
     */
    public  SunAppServer getSunAppServer(){
    	     return (SunAppServer)getServer().getAdapter(SunAppServer.class);
    }
    
    protected void setupLaunch(ILaunch launch, String launchMode, IProgressMonitor monitor) throws CoreException {
        int state = getServer().getServerState();
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunch state=" +state);
        
        SunAppServer  sunserver = getSunAppServer();
       if (sunserver.isRunning()){
           SunAppSrvPlugin.logMessage("in SunAppServerBehaviour CTOR after sunserver it is running!!!");
           setMode(ILaunchManager.RUN_MODE);
          setServerState(IServer.STATE_STARTED);
            resetStatus(IServer.STATE_STARTED);

        } 
       state = getServer().getServerState();
       if (state!=IServer.STATE_STARTED){
        try {
            super.setupLaunch(launch, launchMode, monitor);
        } catch (CoreException ce) {
            
            
            SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunch  ALREADY STARTED!!!!!!");
            setMode(ILaunchManager.RUN_MODE);
            setServerState(IServer.STATE_STARTED);
            resetStatus(IServer.STATE_STARTED);
            return;
        }
       }
        resetStatus(state);
    }
    
    
    public void publishModule(int kind, int deltaKind, IModule[] module,
            IProgressMonitor monitor) throws CoreException {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour publishModule" +module);
        super.publishModule( kind,  deltaKind, module,  monitor);
       
          
    }
    
    
    protected synchronized void setServerStarted() {
        setServerState(IServer.STATE_STARTED);
    }
    
        /*
         * If the server state is unknown, reset the status to OK
         */
    private void resetStatus(int state) {
        if (state == IServer.STATE_UNKNOWN) {
            setServerStatus(null);
        }
    }
    
    
    protected void terminate() {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour terminate");
        int state = getServer().getServerState();
        if (state == IServer.STATE_STOPPED)
            return;
        
        
        IProcess currentProcess = process;
        
        process = null;
        
        shutdown(state);
        
        try {
            if (currentProcess != null && !currentProcess.isTerminated()) {
                currentProcess.terminate();
                currentProcess = null;
            }
        } catch (Exception e) {
            SunAppSrvPlugin.logMessage("Error "+ e);
        }
    }
    
    
    public String getSunApplicationServerInstallationDirectory(){
    	String path= (String)getRuntimeDelegate().getServerInstanceProperties().get(SunAppServer.ROOTDIR);
       /* SunAppServer  sunserver = (SunAppServer) getServer().getAdapter(SunAppServer.class);
        String d = sunserver.getRootDir();
              SunAppSrvPlugin.logMessage("getSunApplicationServerInstallationDirectory is :"+d);
        return d;*/
    	return path;
    }
    /* return true if the current server is a V3 server
     *  based on the modules dir existence for now
     */
    public boolean isV3(){
    	String loc=getSunApplicationServerInstallationDirectory();
    	return new File(loc+"/modules").exists();
    }
    public String getDomainName(){
        SunAppServer  sunserver = getSunAppServer();
        String d = sunserver.getdomainName();
        if (isEmpty(d)) {
        	d = DEFAULT_DOMAIN_NAME;
        }
        SunAppSrvPlugin.logMessage("Domain Name is :"+d);
        return d;
    }
    public String getDomainDir(){
        SunAppServer  sunserver = getSunAppServer();
        String d = sunserver.getDomainDir();
        if (isEmpty(d)) {
        	d = getDefaultDomainDir();
        }
        SunAppSrvPlugin.logMessage("Domain Dir is :"+d);
        return d;
    }
    private String getDefaultDomainDir() {
    	return getSunApplicationServerInstallationDirectory() + File.separatorChar + 
			DEFAULT_DOMAIN_DIR_NAME;
    }

    private boolean isEmpty(String testString) {
        return ((testString == null) || (testString.trim().length() == 0));
    }
    public String getSunApplicationServerAdminPort(){
        //String port= (String)getRuntimeDelegate().getServerInstanceProperties().get(SunAppServer.ADMINSERVERPORT);
        SunAppServer  sunserver = getSunAppServer();
        SunAppSrvPlugin.logMessage("sunappserver.adminserverportnumber we are looking for this prop value:"+sunserver.getAdminServerPort());
        return sunserver.getAdminServerPort();
    }    
    public void stop(boolean force) {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour stop");
        
        resetStatus(getServer().getServerState());
        int state = getServer().getServerState();
        if (state == IServer.STATE_STOPPED){
            SunAppSrvPlugin.logMessage("in SunAppServerBehaviour ALREADY STOPPED...Wierd");
                      return;
        }
        
        ///shutdown(state);
        stopSunServer();
        setServerState(IServer.STATE_STOPPED);
    }
    private String getScriptExtension(){
        String ret="";
        if (File.separator.equals("\\")) {
            ret= ".bat"; //NOI18N
        }
        return ret;
    }
    
    private  void stopSunServer() {
        // set arguments to be passed to Runtime.exec
        String asadminCmd =  getSunApplicationServerInstallationDirectory()+"/bin/asadmin"+ getScriptExtension();
         
        String arr[] = { asadminCmd,
		           "stop-domain",
		           "--domaindir",
		           getDomainDir(), //quote(getDomainDir()), 
		           getDomainName()
		        };
        
        // stop the SJSAS using Runtime.exec
        asyncExec(arr);
        for (int i=0;i<30;i++){//max 30 seconds for stop.
        try {
            Thread.sleep(1000);
            SunAppSrvPlugin.logMessage("in SunAppServerBehaviour stopping...");
            
            SunAppServer  sunserver = getSunAppServer();
            if (sunserver.isRunning()==false){
                SunAppSrvPlugin.logMessage("in SunAppServerBehaviour really stopped");
                Thread.sleep(2000);//need an extra time to flush
               
            	return;
            }
        } catch (InterruptedException ex) {}
        }
    }

    public void startPingingThread(){
        // ping server to check for startup
        try {
            setServerState(IServer.STATE_STARTING);
            String url = "http://"+getServer().getHost() +":" + getSunApplicationServerAdminPort(); 
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour startPingingThread" +url);

            ping = new PingThread(getServer(), url, this);
        } catch (Exception e) {
            SunAppSrvPlugin.logMessage( "Can't ping for server startup."); 
        }
        
    }
    private void asyncExec(final String[] arr) {
        new Thread(new Runnable() {
            public void run() {
                try {
                    BufferedReader input = new BufferedReader(new InputStreamReader(Runtime.getRuntime().exec(arr).getInputStream()));
                    String line = null;
                    while ((line = input.readLine()) != null) SunAppSrvPlugin.logMessage(">>> " + line);
                    input.close();
                } catch (Exception ex) {
                    SunAppSrvPlugin.logMessage("Error starting/stopping integrated SJSAS:\n" + ex);
                }
            }
        }).start();
    }
    
    protected String getDomainDirWithDomainName() {
    	return getDomainDir().trim() + File.separatorChar + getDomainName();
    }

    // quote the string if it contains spaces.  Might want to expand to all
    // white space (tabs, localized white space, etc.)
    /* comment out for now - there are problems creating a domain
     * to test this with
     */
    /* protected static final String quote(String path) {
    	return path.indexOf(' ') == -1 ? path : "\"" + path + "\"";
    }  */   

    protected String getConfigTypeID() {
        return SunAppSrvPlugin.SUNPLUGIN_ID + ".SunAppServerLaunchConfigurationType";
    }
    
    /**
     * Returns the String name of the stop launch configuration.
     * @return
     */
    protected String getStopLaunchName() {
        return "ExternalLaunchStopper";
    }
    

    protected void setProcess(IProcess newProcess) {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setProcess");
        super.setProcess(newProcess);
    }
    
    
}

