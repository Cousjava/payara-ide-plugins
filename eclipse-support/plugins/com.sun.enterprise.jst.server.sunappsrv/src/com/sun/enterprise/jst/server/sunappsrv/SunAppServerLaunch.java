// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
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
import org.eclipse.jdt.launching.AbstractVMInstall;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;

import com.sun.enterprise.jst.server.sunappsrv.log.GlassFishConsole;
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

    @SuppressWarnings("restriction")
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
        sunserver.SunInitialize();	// force reread of domain info if necessary
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
						"Port conflict: Please stop the server process using the same port as the one used by the Application Server.",
						new RuntimeException(
								"A server process is already running on this port but we cannot determine if it's a GlassFish process (lack of info or credentials)." +
								"If you do not find something else running on this port, check for antivirus software blocking or monitoring this port."),
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
        ProcessBuilder pb=null;
      	 AbstractVMInstall/*IVMInstall*/ vm =(AbstractVMInstall)serverBehavior.getRuntimeDelegate().getVMInstall(); 
         try {
        	monitor.worked(10);

			if (serverBehavior.isV3()) {
				
		     	//needed to see if we force JDK 1.6 (prelude did not need it)
				if (sunserver.isV3Prelude()==false) { //so it is v3
					if (vm.getJavaVersion().startsWith("1.5")
							|| vm.getJavaVersion().startsWith("1.4")) {
						// error: real v3 only works with 1.6 or later.
						abort(
								"GlassFish v3 requires JDK 1.6 or later to run. Please select the correct JDK in the Server properties 'Runtime Environment' section.",
								null,
								IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

					}
				}
		/*		SunAppSrvPlugin.logMessage("AbstractVMInstall getJavaVersion="
						+ vm.getJavaVersion(), null);
				SunAppSrvPlugin.logMessage(
						"AbstractVMInstall getInstallLocation"
								+ vm.getInstallLocation(), null);*/
				
				pb = new ProcessBuilder(vm.getInstallLocation() + "/bin/java",
						"-jar", serverBehavior
								.getSunApplicationServerInstallationDirectory()
								+ "/modules/admin-cli.jar", "start-domain",
						"--domaindir", serverBehavior.getDomainDir(),
						debugFlag, verboseFlag, domain);
				//for mac only: this next env variable has to always match the vm used in the processbuilder, otherwise mac
				// will be confused between jdk 1.5 and 1.6 (google search for JAVA_JVM_VERSION for details
				
				pb.environment().put("JAVA_JVM_VERSION", vm.getJavaVersion());
				pb.environment().put("JAVA_HOME", vm.getInstallLocation().getAbsolutePath());
				
				//how do we stop it at exit???
				String[] stopcmd= new String[]{
						vm.getInstallLocation() + "/bin/java",
						"-jar", 
						serverBehavior.getSunApplicationServerInstallationDirectory()
											+ "/modules/admin-cli.jar", 
						"stop-domain",
						"--domaindir", 
						serverBehavior.getDomainDir(),
						 domain						
				};
	              SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(stopcmd);
				
			}
			else {
	        	  pb = new ProcessBuilder(asadminCmd, "start-domain",
						"--domaindir", serverBehavior.getDomainDir(),
						debugFlag, verboseFlag, domain);
	              SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(serverBehavior.getStopCommand());

			}
       	 
        	/* Map<String,String> m= pb.environment();
        	 Set<Map.Entry<String,String>> cc=m.entrySet();
        	 for (Map.Entry<String,String> me : cc){
 				SunAppSrvPlugin.logMessage("map="+me.getKey()+"---"+me.getValue(),null);
     		 
        	 }*/    	 

        	 pb.directory(new File(serverBehavior.getSunApplicationServerInstallationDirectory()));
        	 Process process = pb.start();
                 	       	 
            
            IProcess runtimeProcess = new RuntimeProcess(launch, process, "...", null);


        } catch (IOException ioe) {
            abort("error Launching Executable", ioe,  IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
        }
    	boolean javaDBStart= store.getBoolean(PreferenceConstants.ENABLE_START_JAVADB);
    	if (javaDBStart) {
			String sampleDBDir = serverBehavior.getSampleDatabaseDir();
			if ((serverBehavior.isV3()) && (sunserver.isV3Prelude()==false)) { //so it is v3

				command = ((sampleDBDir == null) ? new String[] {
						vm.getInstallLocation() + "/bin/java",
						"-jar",
						serverBehavior
								.getSunApplicationServerInstallationDirectory()
								+ "/modules/admin-cli.jar", "start-database" }
						: new String[] {
								vm.getInstallLocation() + "/bin/java",
								"-jar",
								serverBehavior
										.getSunApplicationServerInstallationDirectory()
										+ "/modules/admin-cli.jar",
								"start-database", "--dbhome", sampleDBDir

						});
				//add also the stop on exit command:
				SunAppSrvPlugin
						.getInstance()
						.addCommandToExecuteAtExit(
								new String[] {
										vm.getInstallLocation() + "/bin/java",
										"-jar",
										serverBehavior
												.getSunApplicationServerInstallationDirectory()
												+ "/modules/admin-cli.jar",
										"stop-database" });
			} else {
				command = ((sampleDBDir == null) ? new String[] { asadminCmd,
						"start-database" } : new String[] { asadminCmd,
						"start-database", "--dbhome", sampleDBDir });
				//stop the db on exit of the IDE:
				SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(
						new String[] { asadminCmd, "stop-database" });

			}
            try {
				Process process = Execute.launch(null, command, null, new File(serverBehavior.getSunApplicationServerInstallationDirectory()), true);
		           			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
   	}

        boolean viewLog=false;

        for (int i=0;i<120;i++){//max 60 seconds for start.
            try {
            	monitor.worked(10);
               Thread.sleep(500);//1/2 secs
                monitor.worked(i);
                if (viewLog==false){
                	viewLog = true;//view it only once.
                	try {
                		PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
                			public void run() {
            				   	String logFile =  serverBehavior.getDomainDirWithDomainName()+"/logs/server.log";	        
                  				GlassFishConsole.showConsole(new File(logFile));
                			}
                		});



                	}catch (Exception e){
                        SunAppSrvPlugin.logMessage("page.showView",e);
                		
                	}


                }
                if (sunserver.isRunning()){
                	if (serverBehavior.isV3()) {
                		SunAppServer.ServerStatus s=sunserver.getV3ServerStatus();
                		if (s==SunAppServer.ServerStatus.CREDENTIAL_ERROR){
                            
                            abort("Wrong user name or password.", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
                			return;
                		}
                		if (s!=SunAppServer.ServerStatus.DOMAINDIR_MATCHING){
             
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
