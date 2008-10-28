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

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.eclipse.jdt.internal.debug.core.model.JDIDebugTarget;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;

import com.sun.enterprise.jst.server.sunappsrv.log.LogView;
import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;



public class SunAppServerLaunch extends AbstractJavaLaunchConfigurationDelegate {
    
    public static final String GFV3_MODULES_DIR_NAME = "modules"; // NOI18N

    
    public SunAppServerLaunch(){
        // SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch ctor");
    }
    protected void abort(String message, Throwable exception, int code) throws CoreException {
        throw new CoreException(new Status(IStatus.ERROR,  SunAppSrvPlugin.SUNPLUGIN_ID, code, message, exception));
    }
    
    private String getScriptExtension(){
        String ret="";
   	    if (File.separator.equals("\\")) {
		ret= ".bat"; //NOI18N
            }
        return ret;
    }

    public void launch(ILaunchConfiguration configuration,  String mode, ILaunch launch, IProgressMonitor monitor) throws CoreException {
        SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch launch");

        String command[]=null;
        IServer server = ServerUtil.getServer(configuration);
        if (server == null) {
            abort("missing Server", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
        }
       
        final SunAppServerBehaviour serverBehavior = (SunAppServerBehaviour) server.loadAdapter(ServerBehaviourDelegate.class, null);

        SunAppServer sunserver = serverBehavior.getSunAppServer();
        SunAppServer.ServerStatus status =SunAppServer.ServerStatus.CONNEXTION_ERROR ;
        if (sunserver.isRunning()) {
			if (serverBehavior.isV3()) {
				SunAppSrvPlugin
						.logMessage("in SUN SunAppServerLaunch Forcing a STOP!!!!");
				status = sunserver.getV3ServerStatus();
			} else { // V2
				SunAppSrvPlugin
						.logMessage("in SUN SunAppServerLaunch Forcing a STOP!!!!");
				status = sunserver.getV2ServerStatus();
			}
			if (status == SunAppServer.ServerStatus.DOMAINDIR_MATCHING) {
				// we are really to the server we know about, so that we can
				// stop it and restart it to get the log file
				serverBehavior.stop(true);
                try {
					Thread.sleep(2000);
				} catch (InterruptedException e) {
					//e.printStackTrace();
				}

			} else if (status == SunAppServer.ServerStatus.DOMAINDIR_NOT_MATCHING) {
				abort(
						"Please, check the other GlassFish Application Server process and stop it.",
						new RuntimeException(
								"A GlassFish Enterprise Server is running on this port, but with a different root installation..."),
						IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

			} else {
				abort(
						"Port conflict: Please stop the process using the same port as the one used by the Application Server.",
						new RuntimeException(
								"An unknow process is already running on this port."),
						IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

			}
		}
    	IPreferenceStore store = SunAppSrvPlugin.getInstance().getPreferenceStore();
    	boolean verboseMode= store.getBoolean(PreferenceConstants.ENABLE_START_VERBOSE);
        String verboseFlag="--verbose="+verboseMode;

        
    	String asadminCmd =  serverBehavior.getSunApplicationServerInstallationDirectory()+"/bin/asadmin"+getScriptExtension();	        
        String domain = serverBehavior.getDomainName();
        String debugFlag="--debug=false";
        if (mode.equals("debug")) {
       	 debugFlag="--debug";
        }
        command = new String[]{ asadminCmd,
	           "start-domain",
	           "--domaindir",
	           serverBehavior.getDomainDir(), 
	           debugFlag,
	           verboseFlag,
		           domain
		        };
 
        
        try {
            Process process = Execute.launch(null, command, null, new File(serverBehavior.getSunApplicationServerInstallationDirectory()), true);
            IProcess runtimeProcess = new RuntimeProcess(launch, process, "...", null);
     //       launch.addProcess(runtimeProcess);
     //       serverBehavior.setProcess(runtimeProcess);
            SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(serverBehavior.getStopCommand());
        } catch (IOException ioe) {
            abort("error Launching Executable", ioe,  IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
        }
    	boolean javaDBStart= store.getBoolean(PreferenceConstants.ENABLE_START_JAVADB);
    	if(javaDBStart){
            command = new String[]{ asadminCmd,
     	           "start-database"};    		
            try {
				Process process = Execute.launch(null, command, null, new File(serverBehavior.getSunApplicationServerInstallationDirectory()), true);
		        SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(new String[]{ asadminCmd, "stop-database"});
		           			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
   	}

        boolean viewLog=false;

        for (int i=0;i<120;i++){//max 60 seconds for start.
            try {
                Thread.sleep(500);//1/2 secs
                monitor.worked(i);
                if (viewLog==false){
                	viewLog = true;//view it only once.
                	try {
                		PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
                			public void run() {
                				IWorkbench iw =PlatformUI.getWorkbench();
                				IWorkbenchPage page = iw.
                				getActiveWorkbenchWindow().
                				getActivePage();
                				try {
                					LogView lv = (LogView)page.showView("com.sun.enterprise.jst.server.sunappsrv.log.LogView");
                				   	String logFile =  serverBehavior.getDomainDirWithDomainName()+"/logs/server.log";	        
                				    
                					lv.init(new File(logFile));
								} catch (PartInitException e) {
									// TODO Auto-generated catch block
	                				SunAppSrvPlugin.logMessage("page.showView",e);
								}
                			}
                		});



                	}catch (Exception e){
                        SunAppSrvPlugin.logMessage("page.showView",e);
                		
                	}


                }
                if (sunserver.isRunning()){
                	if (serverBehavior.isV3()) {
                		if (sunserver.getV3ServerStatus()!=SunAppServer.ServerStatus.DOMAINDIR_MATCHING){
                			SunAppSrvPlugin.logMessage("V3 not ready");
                			continue;
                		}
                	} else { //V2: wait a little bit more to make sure V2 admin is initialized
                		if (sunserver.getV2ServerStatus()!=SunAppServer.ServerStatus.DOMAINDIR_MATCHING){
                			SunAppSrvPlugin.logMessage("V2 not ready");
                			continue;
                		}                       

                	}
                	serverBehavior.setStartedState();
                	///////startPingingThread();       
                	setDefaultSourceLocator(launch, configuration);
                	if (mode.equals("debug")) {

                		Map<String, String> arg = new HashMap<String , String>();

                		arg.put("hostname", "localhost"); 
                		arg.put("port", "9009");  
                		arg.put("timeout", "25000");  
                		String connectorId = getVMConnectorId(configuration);
                		IVMConnector connector = null;
                		if (connectorId == null) {
                			connector = JavaRuntime.getDefaultVMConnector();
                		} else {
                			connector = JavaRuntime.getVMConnector(connectorId);
                		}      
                		// connect to VM
                		connector.connect(arg, monitor, launch);

                		IDebugEventSetListener processListener = new IDebugEventSetListener() {
                			public void handleDebugEvents(DebugEvent[] events) {
                				if (events != null) {
                					int size = events.length;
                					for (int i = 0; i < size; i++) {
                						if (events[i].getSource() instanceof JDIDebugTarget){
                							JDIDebugTarget dt = (JDIDebugTarget)events[i].getSource();
                							try {

                								SunAppSrvPlugin.logMessage("JDIDebugTarget="+dt.getName());
                								if ((dt.getName().indexOf("localhost:9009")!=-1) && events[i].getKind() == DebugEvent.TERMINATE) {
                									DebugPlugin.getDefault().removeDebugEventListener(this);
                									serverBehavior.stop(true);
                								}
                							} catch (DebugException e) {
                								// TODO Auto-generated catch block
                								e.printStackTrace();
                							}


                						}

                					}
                				}
                			}
                		};

                		DebugPlugin.getDefault().addDebugEventListener(processListener);
                	}
                	return;
                }
            } catch (InterruptedException ex) {}
        }               
    }


    
}
