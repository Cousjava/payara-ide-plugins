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
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.eclipse.jdt.internal.debug.core.model.JDIDebugTarget;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.AbstractVMInstall;
import org.eclipse.jdt.launching.ExecutionArguments;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMRunner;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.VMRunnerConfiguration;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jst.server.core.ServerProfilerDelegate;
import org.eclipse.osgi.service.resolver.BundleDescription;
import org.eclipse.osgi.service.resolver.State;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;
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

        // test if the server is not there anymore... (scenario: upgrade from 1.1 to 1.2 bundle: no more v2.1
        File serverloc = new File(serverBehavior.getSunApplicationServerInstallationDirectory());
        if (!serverloc.exists()){
            abort(
            		NLS.bind(Messages.serverDirectoryGone,serverloc.getAbsolutePath()),
            		null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
       	
        }
        
        SunAppServer sunserver = serverBehavior.getSunAppServer();
		if (!sunserver.isLocalServer()) {
	        if (sunserver.isRunning()) {
	        	serverBehavior.setStartedState(mode);
	        	return;
	        }else {
            abort("GlassFish Remote Servers cannot be start from this machine.", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
	        }
		}
                
        
        SunAppServer.ServerStatus status = SunAppServer.ServerStatus.CONNEXTION_ERROR;
        sunserver.sunInitialize(); // force reread of domain info if necessary
        if (sunserver.isRunning()) {
            if (serverBehavior.isV3()) {
                SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch Forcing a STOP!!!!"); //$NON-NLS-1$
                status = sunserver.getV3ServerStatus();
            } else { // V2
                SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch Forcing a STOP!!!!"); //$NON-NLS-1$
                status = sunserver.getV2ServerStatus();
            }
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

            } else {
                abort(
                        "Port conflict: Please stop the server process using the same port as the one used by the Application Server.", //$NON-NLS-1$
                        new RuntimeException(
                        "A server process is already running on this port but we cannot determine if it's a GlassFish process (lack of info or credentials)." //$NON-NLS-1$
                        + "If you do not find something else running on this port, check for antivirus software blocking or monitoring this port."), //$NON-NLS-1$
                        IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

            }
        }
        IPreferenceStore store = SunAppSrvPlugin.getInstance().getPreferenceStore();
        boolean verboseMode = store.getBoolean(PreferenceConstants.ENABLE_START_VERBOSE);
        String verboseFlag = "--verbose=" + verboseMode;

        String asadminCmd = serverBehavior.getSunApplicationServerInstallationDirectory() + "/bin/asadmin" + getScriptExtension(); //$NON-NLS-1$
        String domain = serverBehavior.getDomainName();
        String debugFlag = "--debug=false"; //$NON-NLS-1$
        if (mode.equals("debug")) { //$NON-NLS-1$
            debugFlag = "--debug"; //$NON-NLS-1$
        }
        ProcessBuilder pb = null;
        AbstractVMInstall/* IVMInstall */ vm = (AbstractVMInstall) serverBehavior.getRuntimeDelegate().getVMInstall();

        IVMInstall vm2 = verifyVMInstall(configuration);
        IVMRunner runner = vm2.getVMRunner(mode);

        File workingDir = verifyWorkingDirectory(configuration);
        String workingDirName = null;
        if (workingDir != null) {
            workingDirName = workingDir.getAbsolutePath();
        }

        // Program & VM args
        String pgmArgs = getProgramArguments(configuration);
        String vmArgs = getVMArguments(configuration);
        String[] envp = getEnvironment(configuration);

        ExecutionArguments execArgs = new ExecutionArguments(vmArgs, pgmArgs);

        // VM-specific attributes
        Map vmAttributesMap = getVMSpecificAttributesMap(configuration);

        // Classpath
        String[] classpath = getClasspath(configuration);

        //String mainTypeName = tomcatServer.getRuntimeClass();

        // Create VM config
        VMRunnerConfiguration runConfig = new VMRunnerConfiguration("com.sun.enterprise.glassfish.bootstrap.ASMain", classpath); //$NON-NLS-1$
        runConfig.setProgramArguments(execArgs.getProgramArgumentsArray());
        runConfig.setVMArguments(execArgs.getVMArgumentsArray());
        runConfig.setWorkingDirectory(workingDirName);
        runConfig.setEnvironment(envp);
        runConfig.setVMSpecificAttributesMap(vmAttributesMap);

        // Bootpath
        String[] bootpath = getBootpath(configuration);
        if (bootpath != null && bootpath.length > 0) {
            runConfig.setBootClassPath(bootpath);
        }

        setDefaultSourceLocator(launch, configuration);
        ReadDomainInfo di = new ReadDomainInfo(serverBehavior.getSunApplicationServerInstallationDirectory(),
                serverBehavior.getDomainName());
        if (ILaunchManager.PROFILE_MODE.equals(mode)) {
            try {
            	// if the IDE is less than Galileo... don't attempt to do
            	// profiling...
            	//
            	Version tptpVersion = getCurrentVersion("org.eclipse.tptp.platform.models"); //$NON-NLS-1$
            	Version testVersion = new Version(4,5,9);
            	if (null == tptpVersion) {
                    ProfilerInfoMessage.display(Messages.noProfilersConfigured);
                    di.removeProfilerElements();
            	} else if (tptpVersion.compareTo(testVersion) < 1) {
            		// open info dialog
            		ProfilerInfoMessage.display(Messages.profilingUnsupportedInVersion);
                    di.removeProfilerElements();
            	} else {
                    ServerProfilerDelegate.configureProfiling(launch, vm2, runConfig,
                            monitor);
                    String[] vmArgs2 = runConfig.getVMArguments();
                    String nativeLib = "";  //$NON-NLS-1$
                    for (String nameVal : runConfig.getEnvironment()) {
                        String jph = "JAVA_PROFILER_HOME="; //$NON-NLS-1$
                        if (nameVal.startsWith(jph)) {
                            nativeLib = nameVal.substring(jph.length());
                        }
                    }
                    if (vmArgs2 != null && vmArgs2.length == 1) {
                        String orig = vmArgs2[0];
                        String[] broken = orig.split(":"); //$NON-NLS-1$
                        String fixed = broken[0] + ":" + nativeLib + File.separator + broken[1] + ":" + broken[2]; //$NON-NLS-1$
                        vmArgs2[0] = fixed;
                    }
                    di.addProfilerElements(nativeLib, vmArgs2);
                }
            } catch (CoreException ce) {
                ProfilerInfoMessage.display(Messages.noProfilersConfigured);
                di.removeProfilerElements();
            }
        } else {
            di.removeProfilerElements();
        }
        try {
            monitor.worked(10);

            if (serverBehavior.isV3()) {

                // needed to see if we force JDK 1.6 (prelude did not need it)
                    if (vm.getJavaVersion().startsWith("1.5") || vm.getJavaVersion().startsWith("1.4")) { //$NON-NLS-1$ //$NON-NLS-2$
                        // error: real v3 only works with 1.6 or later.
                        abort("GlassFish v3 requires JDK 1.6 or later to run. Please select the correct JDK in the Server properties 'Runtime Environment' section.", //$NON-NLS-1$
                                null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
                    }
                    String ext = "";//$NON-NLS-1$
                    if (File.separator.equals("\\")) {//$NON-NLS-1$
                        ext = ".exe"; //$NON-NLS-1$
                    }
                    String javac= vm.getInstallLocation().getAbsolutePath() + "/bin/javac"+ext;
                    if (new File(javac).exists()!=true){
                        abort("GlassFish v3 requires a JDK 1.6 and not a JRE. Please add/select the correct JDK in the Server properties 'Runtime Environment' section.", //$NON-NLS-1$
                                null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);                   	
                    }


                pb = new ProcessBuilder(vm.getInstallLocation() + "/bin/java", //$NON-NLS-1$
                        "-jar", serverBehavior.getSunApplicationServerInstallationDirectory() + "/modules/admin-cli.jar", //$NON-NLS-1$
                        "start-domain", "--domaindir", serverBehavior.getDomainDir(), debugFlag, verboseFlag, domain); //$NON-NLS-1$ //$NON-NLS-2$

                // for mac only: this next env variable has to always match the
                // vm used in the processbuilder, otherwise mac
                // will be confused between jdk 1.5 and 1.6 (google search for
                // JAVA_JVM_VERSION for details

                pb.environment().put("JAVA_JVM_VERSION", vm.getJavaVersion()); //$NON-NLS-1$
                pb.environment().put("JAVA_HOME",vm.getInstallLocation().getAbsolutePath()); //$NON-NLS-1$
                pb.environment().put("AS_JAVA",vm.getInstallLocation().getAbsolutePath()); //$NON-NLS-1$ bug 229

                // how do we stop it at exit???
                String[] stopcmd = new String[]{
                    vm.getInstallLocation() + "/bin/java", "-jar", //$NON-NLS-1$ //$NON-NLS-2$
                    serverBehavior.getSunApplicationServerInstallationDirectory() + "/modules/admin-cli.jar", //$NON-NLS-1$
                    "stop-domain", "--domaindir", serverBehavior.getDomainDir(), domain}; //$NON-NLS-1$ //$NON-NLS-2$
                SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(stopcmd);
            } else {
                pb = new ProcessBuilder(asadminCmd, "start-domain","--domaindir", serverBehavior.getDomainDir(), //$NON-NLS-1$ //$NON-NLS-2$
                        debugFlag, verboseFlag, domain);
                SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(
                        serverBehavior.getStopV2Command());
            }

            pb.directory(new File(serverBehavior.getSunApplicationServerInstallationDirectory()));
            Process process = pb.start();

            IProcess runtimeProcess = new RuntimeProcess(launch, process, "...", null); //$NON-NLS-1$
        } catch (IOException ioe) {
            abort("error Launching Executable", ioe,  IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
        }
        boolean javaDBStart = store.getBoolean(PreferenceConstants.ENABLE_START_JAVADB);
        if (javaDBStart) {
            String sampleDBDir = store.getString(PreferenceConstants.JAVA_DB_LOCATION);
            if (serverBehavior.isV3() ) {  // so it is v3

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
            } else {
                command = ((sampleDBDir == null) ? new String[]{asadminCmd,
                            "start-database"} : new String[]{asadminCmd, //$NON-NLS-1$
                            "start-database", "--dbhome", sampleDBDir}); //$NON-NLS-1$ //$NON-NLS-2$
                // stop the db on exit of the IDE:
                SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(
                        new String[]{asadminCmd, "stop-database"}); //$NON-NLS-1$

            }
            try {
                Process process = Execute.launch(null, command, null, new File(serverBehavior.getSunApplicationServerInstallationDirectory()), true);
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
                Thread.sleep(500);// 1/2 secs
                monitor.worked(i);
                if (viewLog == false) {
                    viewLog = true;// view it only once.
                    try {
                        PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
                                    public void run() {
                                        String logFile = serverBehavior.getDomainDirWithDomainName() + "/logs/server.log"; //$NON-NLS-1$
                                        GlassFishConsole.showConsole(new File(logFile));
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
                    if (serverBehavior.isV3()) {
                        SunAppServer.ServerStatus s = sunserver.getV3ServerStatus();
                        if (s == SunAppServer.ServerStatus.CREDENTIAL_ERROR) {

                            abort("Wrong user name or password.", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
                            return;
                        }
                        if (s != SunAppServer.ServerStatus.DOMAINDIR_MATCHING) {

                            SunAppSrvPlugin.logMessage("V3 not ready"); //$NON-NLS-1$
                            continue;
                        }
                    } else { // V2: wait a little bit more to make sure V2 admin is initialized
                        if (sunserver.getV2ServerStatus() != SunAppServer.ServerStatus.DOMAINDIR_MATCHING) {
                            SunAppSrvPlugin.logMessage("V2 not ready"); //$NON-NLS-1$
                            continue;
                        }

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

                        IDebugEventSetListener processListener = new IDebugEventSetListener() {
                            public void handleDebugEvents(DebugEvent[] events) {
                                if (events != null) {
                                    int size = events.length;
                                    for (int i = 0; i < size; i++) {
                                        if (events[i].getSource() instanceof JDIDebugTarget) {
                                            JDIDebugTarget dt = (JDIDebugTarget) events[i].getSource();
                                            try {

                                                SunAppSrvPlugin.logMessage("JDIDebugTarget=" + dt.getName()); //$NON-NLS-1$
                                                if ((dt.getName().indexOf(
                                                        "localhost:9009") != -1) && events[i].getKind() == DebugEvent.TERMINATE) { //$NON-NLS-1$
                                                    DebugPlugin.getDefault().removeDebugEventListener(
                                                            this);
                                                   /// serverBehavior.stop(true);
                                                   //no need to stop the server at that time see 
                                                  //https://glassfishplugins.dev.java.net/issues/show_bug.cgi?id=263
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
    
    static class ProfilerInfoMessage {
    	static void display(final String mess) {
    		IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
    		final Shell shell;
    		if (null != windows && windows.length > 0) {
    			shell = windows[0].getShell();
    		} else {
    			shell = Display.getDefault().getActiveShell();
    		}

    		shell.getDisplay().asyncExec(new Runnable() {

                public void run() {
                    MessageDialog message;
                    String labels[] = new String[1];
                    labels[0] = Messages.OKButton;
                    message = new MessageDialog(shell, Messages.startupWarning, null, mess, MessageDialog.WARNING, labels, 1);
                    message.open();

                }
            });
    	}
    }
}
