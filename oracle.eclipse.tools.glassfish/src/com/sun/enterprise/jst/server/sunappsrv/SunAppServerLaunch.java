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


package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.AbstractVMInstall;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.osgi.service.resolver.BundleDescription;
import org.eclipse.osgi.service.resolver.State;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;
import org.glassfish.tools.ide.admin.ResultProcess;
import org.glassfish.tools.ide.server.ServerTasks;
import org.glassfish.tools.ide.utils.ServerUtils;
import org.glassfish.tools.ide.utils.Utils;
import org.osgi.framework.Version;

import com.sun.enterprise.jst.server.sunappsrv.log.GlassFishConsole;
import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;

@SuppressWarnings("restriction")
public class SunAppServerLaunch extends AbstractJavaLaunchConfigurationDelegate {

    public static final String GFV3_MODULES_DIR_NAME = "modules"; //$NON-NLS-1$

    public SunAppServerLaunch() {
        // SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch ctor");
    }

    protected void abort(String message, Throwable exception, int code) throws CoreException {
        throw new CoreException(new Status(IStatus.ERROR,  SunAppSrvPlugin.SUNPLUGIN_ID, code, message, exception));
    }

    private String getScriptExtension() {
        String ret = ""; //$NON-NLS-1$
        if (File.separator.equals("\\")) {//$NON-NLS-1$
            ret = ".bat"; //$NON-NLS-1$
        }
        return ret;
    }
    
    private static Version getCurrentVersion(String bundleId) {
    	State s = Platform.getPlatformAdmin().getState();
        if (null == s) {
            return null;
        }
    	BundleDescription bd = s.getBundle(bundleId, null);
    	return  null == bd ? null : bd.getVersion();
    }

	public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch, IProgressMonitor monitor) throws CoreException {
        SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch launch"); //$NON-NLS-1$

        String command[] = null;
        IServer server = ServerUtil.getServer(configuration);
        if (server == null) {
            abort("missing Server", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
        }

        final SunAppServerBehaviour serverBehavior = (SunAppServerBehaviour) server.loadAdapter(ServerBehaviourDelegate.class, null);
        
        final SunAppServer sunserver = serverBehavior.getSunAppServer();
		if (!sunserver.isLocalServer()) {
	        if (sunserver.isRunning()) {
	        	serverBehavior.setStartedState(mode);
	        	return;
	        }else {
	        	abort("GlassFish Remote Servers cannot be start from this machine.", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
	        }
		}
                
        checkServerStatus(serverBehavior);
        String domain = serverBehavior.getDomainName();
        String domainAbsolutePath = serverBehavior.getDomainDir() + File.separator + domain;
              
        File bootstrapJar = ServerUtils.getJarName(serverBehavior.getSunApplicationServerInstallationDirectory(), ServerUtils.GFV3_JAR_MATCHER);
        if(bootstrapJar == null) {
            abort("bootstrap jar not found", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
        }
        
        // TODO which java to use? for now ignore the one from launch config
        AbstractVMInstall/* IVMInstall */ vm = (AbstractVMInstall) serverBehavior.getRuntimeDelegate().getVMInstall();
        //IVMInstall vm2 = verifyVMInstall(configuration);

        
        StartupArgsImpl startArgs  = new StartupArgsImpl();
        startArgs.setJavaHome(vm.getInstallLocation().getAbsolutePath());
     // Program & VM args
        String pgmArgs = getProgramArguments(configuration);
        String vmArgs = getVMArguments(configuration);
        startArgs.addJavaArgs(vmArgs);
        startArgs.addGlassfishArgs(pgmArgs);
        startArgs.addGlassfishArgs("--domain " + domain);
        startArgs.addGlassfishArgs("--domaindir " + Utils.quote(domainAbsolutePath));
        if ("debug".equals(mode)) {
        	startArgs.addJavaArgs("-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=9009");
        }
        
        //String[] envp = getEnvironment(configuration);
        
        IPreferenceStore store = SunAppSrvPlugin.getInstance().getPreferenceStore();
//        boolean verboseMode = store.getBoolean(PreferenceConstants.ENABLE_START_VERBOSE);

        setDefaultSourceLocator(launch, configuration);

        ResultProcess process = ServerTasks.startServer(sunserver, startArgs);
        
        //DebugPlugin.newProcess(launch, process.getValue().getProcess(), "GlassFish Server");
                
        boolean javaDBStart = store.getBoolean(PreferenceConstants.ENABLE_START_JAVADB);
        if (javaDBStart) {
            String sampleDBDir = store.getString(PreferenceConstants.JAVA_DB_LOCATION);
                command = ((sampleDBDir == null) ? new String[]{
                            vm.getInstallLocation() + "/bin/java", //$NON-NLS-1$
                            "-jar", //$NON-NLS-1$
                            serverBehavior.getSunApplicationServerInstallationDirectory() + "/modules/admin-cli.jar", "start-database"} //$NON-NLS-1$ //$NON-NLS-2$
                        : new String[]{
                            vm.getInstallLocation() + "/bin/java", //$NON-NLS-1$
                            "-jar", //$NON-NLS-1$
                            serverBehavior.getSunApplicationServerInstallationDirectory() + "/modules/admin-cli.jar", //$NON-NLS-1$
                            "start-database", "--dbhome", sampleDBDir //$NON-NLS-1$ //$NON-NLS-2$
                        });
                // add also the stop on exit command:
                SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(
                        new String[]{
                            vm.getInstallLocation() + "/bin/java", //$NON-NLS-1$
                            "-jar", //$NON-NLS-1$
                            serverBehavior.getSunApplicationServerInstallationDirectory() + "/modules/admin-cli.jar", //$NON-NLS-1$
                            "stop-database"}); //$NON-NLS-1$
            try {
                Process process2 = Execute.launch(null, command, null, new File(serverBehavior.getSunApplicationServerInstallationDirectory()), true);
                DebugPlugin.newProcess(launch, process2, "Derby Database");
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        boolean viewLog = false;
        int timeout = server.getStartTimeout();
        for (int i = 0; i < timeout; i++) {
            try {
                monitor.worked(10);
                Thread.sleep(1000);// 1 sec
                monitor.worked(1);
                if (viewLog == false) {
                    viewLog = true;// view it only once.
                    try {
                        PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
                                    public void run() {
                                        String logFile = serverBehavior.getDomainDirWithDomainName() + "/logs/server.log"; //$NON-NLS-1$
                                        GlassFishConsole.showConsole(sunserver);
                                    }
                                });
                    } catch (Exception e) {
                        SunAppSrvPlugin.logMessage("page.showView", e); //$NON-NLS-1$
                    }
                }
                //if (ILaunchManager.PROFILE_MODE.equals(mode)) {
                	//return;
                //}
                if (sunserver.isRunning()) {

                	SunAppServer.ServerStatus s = sunserver.getServerStatus();
                	if (s == SunAppServer.ServerStatus.CREDENTIAL_ERROR) {

                		abort("Wrong user name or password.", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
                		return;
                	}
                	if (s != SunAppServer.ServerStatus.DOMAINDIR_MATCHING) {

                		SunAppSrvPlugin.logMessage("V3 not ready"); //$NON-NLS-1$
                		continue;
                	}

                	serverBehavior.setStartedState(mode);
                    // /////startPingingThread();
                    setDefaultSourceLocator(launch, configuration);
                    if (mode.equals("debug")) { //$NON-NLS-1$

                        Map<String, String> arg = new HashMap<String, String>();

                        arg.put("hostname", "localhost"); //$NON-NLS-1$ //$NON-NLS-2$
                        arg.put("port", "9009"); //$NON-NLS-1$ //$NON-NLS-2$
                        arg.put("timeout", "25000"); //$NON-NLS-1$ //$NON-NLS-2$
                        String connectorId = getVMConnectorId(configuration);
                        IVMConnector connector = null;
                        if (connectorId == null) {
                            connector = JavaRuntime.getDefaultVMConnector();
                        } else {
                            connector = JavaRuntime.getVMConnector(connectorId);
                        }
                        // connect to VM
                        connector.connect(arg, monitor, launch);
                        
                    }
                    return;
                }
            } catch (InterruptedException ex) {}
        }
    }
	
//	private void attachDebugger(String host, int debugPort, ILaunchConfiguration configuration) {
//		Map<String, String> arg = new HashMap<String, String>();
//
//        arg.put("hostname", "localhost"); //$NON-NLS-1$ //$NON-NLS-2$
//        arg.put("port", "9009"); //$NON-NLS-1$ //$NON-NLS-2$
//        arg.put("timeout", "25000"); //$NON-NLS-1$ //$NON-NLS-2$
//        String connectorId = getVMConnectorId(configuration);
//        IVMConnector connector = null;
//        if (connectorId == null) {
//            connector = JavaRuntime.getDefaultVMConnector();
//        } else {
//            connector = JavaRuntime.getVMConnector(connectorId);
//        }
//        // connect to VM
//        connector.connect(arg, monitor, launch);
//	}
	
//	private void waitForStart(SunAppServer server, final SunAppServerBehaviour serverBehavior, IProgressMonitor monitor) {
//		boolean viewLog = false;
//		int i = 0;
//		while (true) {
//			try {
//                monitor.worked(10);
//                Thread.sleep(500);// 1/2 secs
//                monitor.worked(i++);
//                if (viewLog == false) {
//                    viewLog = true;// view it only once.
//                    try {
//                        PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
//                                    public void run() {
//                                        String logFile = serverBehavior.getDomainDirWithDomainName() + "/logs/server.log"; //$NON-NLS-1$
//                                        GlassFishConsole.showConsole(new File(logFile));
//                                    }
//                                });
//                    } catch (Exception e) {
//                        SunAppSrvPlugin.logMessage("page.showView", e); //$NON-NLS-1$
//                    }
//                }
//                //if (ILaunchManager.PROFILE_MODE.equals(mode)) {
//                	//return;
//                //}
//                if (server.isRunning()) {
//
//                	SunAppServer.ServerStatus s = sunserver.getServerStatus();
//                	if (s == SunAppServer.ServerStatus.CREDENTIAL_ERROR) {
//
//                		abort("Wrong user name or password.", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
//                		return;
//                	}
//                	if (s != SunAppServer.ServerStatus.DOMAINDIR_MATCHING) {
//
//                		SunAppSrvPlugin.logMessage("V3 not ready"); //$NON-NLS-1$
//                		continue;
//                	}
//
//                	serverBehavior.setStartedState(mode);
//                    
//                }
//            } catch (InterruptedException ex) {}
//		}
//	}
	
	private void checkServerStatus(SunAppServerBehaviour serverBehavior) throws CoreException {
		SunAppServer.ServerStatus status = SunAppServer.ServerStatus.CONNEXTION_ERROR;
		SunAppServer sunserver = serverBehavior.getSunAppServer();
        sunserver.readDomainConfig(); // force reread of domain info if necessary
        if (sunserver.isRunning()) {
        	status = sunserver.getServerStatus();
            if (status == SunAppServer.ServerStatus.DOMAINDIR_MATCHING) {
                // we are really to the server we know about, so that we can
                // stop it and restart it to get the log file
                serverBehavior.stop(true);
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e) {
                    // e.printStackTrace();
                }

            } else if (status == SunAppServer.ServerStatus.DOMAINDIR_NOT_MATCHING) {
                abort(
                        "Please, check the other GlassFish Application Server process and stop it.", //$NON-NLS-1$
                        new RuntimeException(
                        "A GlassFish Enterprise Server is running on this port, but with a different root installation..."),//$NON-NLS-1$
                        IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

            } else if (status == SunAppServer.ServerStatus.CONNEXTION_ERROR) {
                abort(
                        "The Eclipse plugin cannot communicate with the GlassFish server....", //$NON-NLS-1$
                        new RuntimeException(
                        "The Eclipse plugin cannot communicate with the GlassFish server." //$NON-NLS-1$
                        + "Please, check for antivirus software blocking or monitoring this port, or firewall configuration, or VPN setup which might block some ports..."), //$NON-NLS-1$
                        IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

            } else {
                abort(
                        "The Eclipse plugin cannot communicate with the GlassFish server, status is :" + status, //$NON-NLS-1$
                        new RuntimeException(
                        "The Eclipse plugin cannot communicate with the GlassFish server." //$NON-NLS-1$
                        + "Status is :"+status), //$NON-NLS-1$
                        IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);           	
            }
        }
	}
	
}
