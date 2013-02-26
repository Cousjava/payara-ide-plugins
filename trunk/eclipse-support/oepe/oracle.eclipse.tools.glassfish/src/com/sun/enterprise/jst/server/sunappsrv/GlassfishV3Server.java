package com.sun.enterprise.jst.server.sunappsrv;

import org.glassfish.tools.ide.data.GlassFishVersion;

public class GlassfishV3Server extends GlassfishGenericServer {

	@Override
	public GlassFishVersion getVersion() {
		return GlassFishVersion.GF_3;
	}

}
