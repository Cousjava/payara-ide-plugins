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
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;



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
       
        SunAppServerBehaviour serverBehavior = (SunAppServerBehaviour) server.loadAdapter(ServerBehaviourDelegate.class, null);

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
				status = sunserver.getV3ServerStatus();
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
	           "--verbose",
		           domain
		        };
 
        
        try {
            Process process = Execute.launch(null, command, null, new File(serverBehavior.getSunApplicationServerInstallationDirectory()), true);
            IProcess runtimeProcess = new RuntimeProcess(launch, process, "...", null);
            launch.addProcess(runtimeProcess);
            serverBehavior.setProcess(runtimeProcess);
        } catch (IOException ioe) {
            abort("error Launching Executable", ioe,  IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
        }

        for (int i=0;i<120;i++){//max 60 seconds for start.
            try {
                Thread.sleep(500);//1/2 secs
                monitor.worked(1);
                             
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
                    serverBehavior.startPingingThread();       
                    setDefaultSourceLocator(launch, configuration);
                    if (mode.equals("debug")) {

                   Map<String, String> argMap = new HashMap<String , String>();
                    
                   argMap.put("hostname", "localhost"); 
                   argMap.put("port", "9009");  
                   argMap.put("timeout", "25000");  
                    String connectorId = getVMConnectorId(configuration);
                    IVMConnector connector = null;
                    if (connectorId == null) {
                        connector = JavaRuntime.getDefaultVMConnector();
                    } else {
                        connector = JavaRuntime.getVMConnector(connectorId);
                    }      
                    // connect to remote VM
                    connector.connect(argMap, monitor, launch);
                    }
                	return;
                }
            } catch (InterruptedException ex) {}
            }               

    }
 
 
    
}
