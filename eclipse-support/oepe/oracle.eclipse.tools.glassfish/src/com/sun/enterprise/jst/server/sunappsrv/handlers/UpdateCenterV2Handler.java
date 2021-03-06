package com.sun.enterprise.jst.server.sunappsrv.handlers;

import java.io.File;
import java.io.IOException;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class UpdateCenterV2Handler extends AbstractGlassfishSelectionHandler {

	@Override
	public void processSelection(IStructuredSelection selection) {
		IServer server = (IServer) selection.getFirstElement();
		GlassfishGenericServer serverAdapter = (GlassfishGenericServer) server
				.loadAdapter(GlassfishGenericServer.class, null);
		String loc=serverAdapter.getServerInstallationDirectory() + "/updatecenter/bin/updatetool";    
   	    if (File.separator.equals("\\")) {
   			loc = loc + ".bat"; //NOI18N
   	    }
		String[] command = new String[]{
				loc    		}; 
		try {
			File appinstallDir = new File(serverAdapter.getServerInstallationDirectory());

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
	}

}
