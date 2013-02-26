package com.sun.enterprise.jst.server.sunappsrv;

import org.glassfish.tools.ide.data.GlassFishAdminInterface;
import org.glassfish.tools.ide.data.GlassFishVersion;

public class GlassfishV4Server extends GlassfishGenericServer {

	@Override
	public GlassFishVersion getVersion() {
		return GlassFishVersion.GF_4;
	}

	@Override
	public GlassFishAdminInterface getAdminInterface() {
		return GlassFishAdminInterface.HTTP;
	}

	
}
