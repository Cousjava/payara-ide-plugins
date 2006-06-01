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
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jst.server.generic.core.internal.GenericServerBehaviour;
import org.eclipse.jst.server.generic.core.internal.PingThread;
import org.eclipse.wst.server.core.IServer;

/**
 *
 *
 */
public class SunAppServerBehaviour extends GenericServerBehaviour {
    
    /** Creates a new instance of SunAppServerBehaviour */
    public SunAppServerBehaviour() {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour CTOR ");
    }
    
    
    private ILaunchConfigurationWorkingCopy wc;
    private String mode;
    private ILaunch launch;
    private IProgressMonitor monitor;


    
    
    
    protected void setupLaunch(ILaunch launch, String launchMode, IProgressMonitor monitor) throws CoreException {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunch");
        int state = getServer().getServerState();
        
        try {
            super.setupLaunch(launch, launchMode, monitor);
        } catch (CoreException ce) {
            
            
            SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunch  ALREADY STARTED!!!!!!");
            setMode(ILaunchManager.RUN_MODE);
            setServerState(IServer.STATE_STARTED);
            resetStatus(IServer.STATE_STARTED);
            return;
        }
        resetStatus(state);
    }
    
    
    
    
    
    protected synchronized void setServerStarted() {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setServerStarted");
        if (wc != null) {
            try {
                SunAppServerLaunch.startDebugging(wc, mode, launch, monitor);
            } catch (CoreException ce) {
                
                setMode(ILaunchManager.RUN_MODE);
            } finally {
                clearDebuggingConfig();
            }
        }
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
        SunAppSrvPlugin.logMessage("sunappserver.rootdirectory we are looking for this prop value:"+path);
        return path;
    }
    public String getSunApplicationServerAdminPort(){
        //String port= (String)getRuntimeDelegate().getServerInstanceProperties().get(SunAppServer.ADMINSERVERPORT);
        SunAppServer  sunserver = (SunAppServer) getServer().getAdapter(SunAppServer.class);
        SunAppSrvPlugin.logMessage("sunappserver.adminserverportnumber we are looking for this prop value:"+sunserver.getAdminServerPort());
        return sunserver.getAdminServerPort();
    }    
    public void stop(boolean force) {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour stop");
        
        resetStatus(getServer().getServerState());
        int state = getServer().getServerState();
        if (state == IServer.STATE_STOPPED)
            return;
        
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
        String domain = "domain1";
        String arr[] = { asadminCmd,
        "stop-domain",
        domain
        };
        
        // stop the SJSAS using Runtime.exec
        asyncExec(arr);
        try {
            Thread.sleep(3000);
        } catch (InterruptedException ex) {}
    }
    private void startSunServer() {
        
        // set arguments to be passed to Runtime.exec
        String asadminCmd =  getSunApplicationServerInstallationDirectory()+"/bin/asadmin"+ getScriptExtension();
        String domain = "domain1";
        String arr[] = { asadminCmd,
        "start-domain",
        
        domain
        };
        
        // stop the SJSAS using Runtime.exec
        asyncExec(arr);
        try {
            Thread.sleep(10000);
        } catch (InterruptedException ex) {}
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
    
    
    
    
    
    protected synchronized void setDebuggingConfig(ILaunchConfigurationWorkingCopy wc,    String mode,   ILaunch launch,  IProgressMonitor monitor) {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setDebuggingConfig");
        this.wc = wc;
        this.mode = mode;
        this.launch = launch;
        this.monitor = monitor;
    }
    
    private synchronized void clearDebuggingConfig() {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour clearDebuggingConfig");
        this.wc = null;
        this.mode = null;
        this.launch = null;
        this.monitor = null;
    }
    
    protected void setProcess(IProcess newProcess) {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setProcess");
        super.setProcess(newProcess);
    }
    
    
}

