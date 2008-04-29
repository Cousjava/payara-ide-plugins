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

package com.sun.enterprise.jst.server.sunappsrv.sunresource;

import org.eclipse.datatools.connectivity.IConnectionProfile;
import org.eclipse.datatools.connectivity.drivers.jdbc.IJDBCDriverDefinitionConstants;

public class JDBCInfo {
	private IConnectionProfile connectionProfile;
	private UrlData urlDataParser;

	/**
	 * Constructor for JDBCInfo.
	 * 
	 * @param profile
	 */
	public JDBCInfo(IConnectionProfile profile) {
		connectionProfile = profile;
		urlDataParser = new UrlData(getURL());
	}

	private String getProperty(String propName) {
		if (connectionProfile != null) {
			return connectionProfile.getBaseProperties().getProperty(propName);
		}
		return null;
	}	
	public String getUserName() {
		return getProperty(IJDBCDriverDefinitionConstants.USERNAME_PROP_ID);
	}

	public String getUserPassword() {
		return getProperty(IJDBCDriverDefinitionConstants.PASSWORD_PROP_ID);
	}

	public String getURL() {
		return getProperty(IJDBCDriverDefinitionConstants.URL_PROP_ID);
	}

	public String getDriverClass() {
		return getProperty(IJDBCDriverDefinitionConstants.DRIVER_CLASS_PROP_ID);
	}

	public String getDatabaseVendor() {
		return getProperty(IJDBCDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID);
	}

	public String getPort() {
		return urlDataParser.getPort();
	}

	public String getServerName() {
		return urlDataParser.getHostName();
	}

	public String getDatabaseName() {
		return getProperty(IJDBCDriverDefinitionConstants.DATABASE_NAME_PROP_ID);
	}
}
