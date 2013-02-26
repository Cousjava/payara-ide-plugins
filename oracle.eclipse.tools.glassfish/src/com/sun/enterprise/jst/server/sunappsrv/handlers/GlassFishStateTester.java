package com.sun.enterprise.jst.server.sunappsrv.handlers;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.wst.server.core.IServer;

public class GlassFishStateTester extends PropertyTester {

	@Override
	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		IServer server = (IServer) receiver;
		return (server.getServerState() == IServer.STATE_STARTED);
//		SunAppServer sunServer = (SunAppServer)server.getAdapter(SunAppServer.class);
//		if (sunServer == null) {
//			sunServer = (SunAppServer) server.loadAdapter(SunAppServer.class, new NullProgressMonitor());
//		}
//		try {
//			return sunServer.isRunning();
//		} catch (CoreException e) {
//			SunAppSrvPlugin.logMessage("Testing server state failed", e);
//		}
//		return false;
	}

}
