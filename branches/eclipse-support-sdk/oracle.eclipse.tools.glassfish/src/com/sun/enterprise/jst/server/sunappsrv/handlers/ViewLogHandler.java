package com.sun.enterprise.jst.server.sunappsrv.handlers;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.wst.server.core.IServer;
import org.glassfish.tools.ide.server.FetchLog;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.log.GlassfishConsoleManager;
import com.sun.enterprise.jst.server.sunappsrv.log.IGlassFishConsole;

public class ViewLogHandler extends AbstractGlassfishSelectionHandler {

	@Override
	public void processSelection(IStructuredSelection selection) {
		IServer server = (IServer) selection.getFirstElement();
		try {
			GlassfishGenericServer serverAdapter = (GlassfishGenericServer) server
					.loadAdapter(GlassfishGenericServer.class, null);

			if (serverAdapter.isRemote()
					&& !serverAdapter.getServerBehaviourAdapter().isRunning()) {
				showMessageDialog();
				return;
			}

			IGlassFishConsole console = GlassfishConsoleManager
					.showConsole(serverAdapter);
			console.startLogging(FetchLog.create(serverAdapter, true));
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage("Error opening log: " + e.getMessage());

		}
	}

}
