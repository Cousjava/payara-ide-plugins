package com.sun.enterprise.jst.server.sunappsrv.handlers;

import java.io.File;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;

public class GlassFishVersionTester extends PropertyTester {

	@Override
	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		IServer server = (IServer) receiver;
		GlassfishGenericServer serverAdapter = (GlassfishGenericServer) server
				.loadAdapter(GlassfishGenericServer.class, null);
		return new File(serverAdapter.getServerInstallationDirectory() + "/modules").exists();
	}

}
