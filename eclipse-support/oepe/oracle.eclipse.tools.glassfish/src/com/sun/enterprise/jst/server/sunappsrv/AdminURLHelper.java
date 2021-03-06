/*
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
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

import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.serverview.Utils;

public class AdminURLHelper {
	private static final String fallbackHost = "localhost";  //$NON-NLS-1$
	private static final String fallbackPort = "4848";       //$NON-NLS-1$

	private AdminURLHelper() {
		super();
	}

	public static String getURL(String urlSuffix, IServer server) {
		String hostName = fallbackHost;
		String portNumber = fallbackPort;

	    if (server != null){
			GlassfishGenericServerBehaviour sab = (GlassfishGenericServerBehaviour)server.loadAdapter(
					GlassfishGenericServerBehaviour.class, null);
			GlassfishGenericServer sunserver = sab.getSunAppServer();
			hostName = sunserver.getServer().getHost();
			portNumber = sunserver.getAdminServerPort();
	    }
	    return Utils.getHttpListenerProtocol(hostName, portNumber) + "://" + hostName + ":" + portNumber + urlSuffix; //$NON-NLS-1$
	}
}
