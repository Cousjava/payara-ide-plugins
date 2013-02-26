package com.sun.enterprise.jst.server.sunappsrv.handlers;

import java.io.File;
import java.io.IOException;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.externaltools.internal.model.IExternalToolConstants;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class UpdateCenterHandler extends AbstractGlassfishSelectionHandler {

	@Override
	public void processSelection(IStructuredSelection selection) {
		// only one server should be selected
		IServer server = (IServer)selection.getFirstElement();
		SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter( SunAppServerBehaviour.class, null);
    	if (!sab.isV3()) { // V2 only
			String loc=sab.getSunApplicationServerInstallationDirectory()+"/updatecenter/bin/updatetool";    
       	    if (File.separator.equals("\\")) {
       			loc = loc + ".bat"; //NOI18N
       	    }
    		String[] command = new String[]{
    				loc    		}; 
    		try {
    			File appinstallDir = new File(sab.getSunApplicationServerInstallationDirectory());
 
    			// need to test this separately because in this case the launch below doesn't work, but
    			// the exit code is not a failure
    			if (!appinstallDir.canWrite()) {
    				throw new IOException("Cannot write to directory " + appinstallDir);
    			}

    			Process process = Execute.launch(null, command, null, appinstallDir, true);
    			try {
    				int exitValue = process.exitValue();

    				// this test works in debug with breakpoints, but not regular run for certain cases - could
    				// be a timing issue because same "failure" case exits with 1 or 0 depending on timing
    				if (Execute.isFailure(exitValue)) {
    					// can get fancy here and try to get info out of the process error stream
    					// on why it failed, but it is not straightforward and this is a corner case
    					throw new IOException();
    				}
    			} catch (IllegalThreadStateException e) {
    				// if it gets here, the ui came up and is still running, we don't want to 
    				// block - this is actually the "success" case
    			}
    			return;
    		} catch (Exception ioe) {
    			String exceptionMessage = ioe.getMessage();
    			String message = ((exceptionMessage != null) ? "Error launching updatetool executable: " + exceptionMessage :
    				"Error launching updatetool executable");
    			showMessageDialog(message);
    			SunAppSrvPlugin.logMessage("error Launching Executable", ioe);
    		}
    	} else { // V3
			File installRoot = new File(sab.getSunApplicationServerInstallationDirectory()).getParentFile();
			if (!isUCInstalled(installRoot)){
				showMessageDialog("GlassFish Update Tool is not yet installed. Please read the Eclipse console output and there, type 'y' to start the installation...");
				
			}
			File tool= getV3UpdateCenterLauncher (installRoot);
			// UC launcher was not found - tell it to the user
			if (tool == null) {
				showMessageDialog("Selected Glassfish server installation does not contain update center launcher.");
				return;
			}
			try {

				ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
				ILaunchConfigurationType programType =
					manager.getLaunchConfigurationType(IExternalToolConstants.ID_PROGRAM_LAUNCH_CONFIGURATION_TYPE);

				ILaunchConfiguration cfg = programType.newInstance(null, "updatecentertool");
				ILaunchConfigurationWorkingCopy wc = cfg.getWorkingCopy();
				wc.setAttribute(IExternalToolConstants.ATTR_LOCATION,tool.getAbsolutePath());
				wc.setAttribute(IExternalToolConstants.ATTR_WORKING_DIRECTORY, installRoot.getAbsolutePath());
				wc.setAttribute(IExternalToolConstants.ATTR_TOOL_ARGUMENTS, "");
				cfg = wc.doSave();
			//	ILaunch il = cfg.launch(ILaunchManager.RUN_MODE, null, false, true);
				DebugUITools.launch(cfg, ILaunchManager.RUN_MODE);
				
				cfg.delete();
			} catch (CoreException e) {
				//TODO: log error
				e.printStackTrace();
			}
    	}
	}
	
	private boolean isUCInstalled(File installRoot) {
        return new File(installRoot, "updatetool/bin").exists();
    }
    
    /**
     * Locate update center launcher within the glassfish installation
     *   [installRoot]/updatecenter/bin/updatetool[.BAT]
     * 
     * @param asInstallRoot appserver install location
     * @return File reference to launcher, or null if not found.
     */
    private File getV3UpdateCenterLauncher(File installRoot) {
        File result = null;
        if(installRoot != null && installRoot.exists()) {
            File updateCenterBin = new File(installRoot, "bin"); // NOI18N
            if(updateCenterBin.exists()) {
           	    if (File.separator.equals("\\")) {
                    File launcherPath = new File(updateCenterBin, "updatetool.exe"); // NOI18N
                    if(launcherPath.exists()) {
                        result = launcherPath;
                    } else {
                        launcherPath = new File(updateCenterBin, "updatetool.bat"); // NOI18N
                        result = (launcherPath.exists()) ? launcherPath : null;
                    }
                } else {
                    File launcherPath = new File(updateCenterBin, "updatetool"); // NOI18N
                    result = (launcherPath.exists()) ? launcherPath : null;
                    if (result!=null){
                    	result.setExecutable(true);
                    }
                }
            }
        }
        return result;
    }

}
