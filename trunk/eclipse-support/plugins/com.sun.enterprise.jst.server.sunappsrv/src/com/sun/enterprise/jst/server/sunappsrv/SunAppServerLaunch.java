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
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.eclipse.jdt.internal.launching.JavaRemoteApplicationLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jst.server.generic.core.internal.GenericServerCoreMessages;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;



public class SunAppServerLaunch extends AbstractJavaLaunchConfigurationDelegate {
    
    
    
    private static JavaRemoteApplicationLaunchConfigurationDelegate debuggingDelegate =
            new JavaRemoteApplicationLaunchConfigurationDelegate();
    
    public SunAppServerLaunch(){
         SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch ctor");
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
        
       ////////  if (mode.equals("debug")) ...
        
        IServer server = ServerUtil.getServer(configuration);
        if (server == null) {
            abort(GenericServerCoreMessages.missingServer, null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
        }
        
        SunAppServerBehaviour serverBehavior = (SunAppServerBehaviour) server.loadAdapter(ServerBehaviourDelegate.class, null);
        String asadminCmd =  serverBehavior.getSunApplicationServerInstallationDirectory()+"/bin/asadmin"+getScriptExtension();
        String domain = "domain1";
        String arr[] = { asadminCmd,
            "start-domain",        
            domain
        };
 
        try {
            Process process = Execute.launch(null, arr, null, new File(serverBehavior.getSunApplicationServerInstallationDirectory()), true);
            IProcess runtimeProcess = new RuntimeProcess(launch, process, "...", null);
            launch.addProcess(runtimeProcess);
            serverBehavior.setProcess(runtimeProcess);
        } catch (IOException ioe) {
            abort(GenericServerCoreMessages.errorLaunchingExecutable, ioe,  IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
        }
        try {
            Thread.sleep(8000);
        } catch (InterruptedException ex) {}
        serverBehavior.startPingingThread();       
        

    }
    
    
    
    protected static void startDebugging(ILaunchConfigurationWorkingCopy wc,  String mode,  ILaunch launch,  IProgressMonitor monitor) throws CoreException {
        debuggingDelegate.launch(wc, mode, launch, monitor);
    }
    
//    private void setDebugArgument(ILaunchConfigurationWorkingCopy config, String attribKey, String key, String arg) {
//        try {
//            Map args = config.getAttribute(attribKey, (Map)null);
//            if (args!=null) {
//                args = new HashMap(args);
//            } else {
//                args = new HashMap();
//            }
//            args.put(key, String.valueOf(arg));
//            config.setAttribute(attribKey, args);
//        } catch (CoreException ce) {
//           
//        }
//    }
    
    
    
}
