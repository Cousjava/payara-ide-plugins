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
//workaround for older DTP and the constants class not present
//import org.eclipse.datatools.connectivity.drivers.jdbc.IJDBCDriverDefinitionConstants;
//end workaround
public class JDBCInfo {
	//workaround for older DTP and the constants class not present
	public static final String PROP_PREFIX = "org.eclipse.datatools.connectivity.db."; //$NON-NLS-1$
	public static final String USERNAME_PROP_ID = PROP_PREFIX + "username"; //$NON-NLS-1$
	public static final String DRIVER_CLASS_PROP_ID = PROP_PREFIX + "driverClass"; //$NON-NLS-1$
	public static final String URL_PROP_ID = PROP_PREFIX + "URL"; //$NON-NLS-1$
	public static final String PASSWORD_PROP_ID = PROP_PREFIX + "password"; //$NON-NLS-1$
	public static final String DATABASE_VENDOR_PROP_ID = PROP_PREFIX + "vendor"; //$NON-NLS-1$
	public static final String DATABASE_NAME_PROP_ID = PROP_PREFIX + "databaseName"; //$NON-NLS-1$
	//end workaround

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
//workaround for older DTP and the constants class not present
//		return getProperty(IJDBCDriverDefinitionConstants.USERNAME_PROP_ID);
		return getProperty(USERNAME_PROP_ID);
//end workaround
	}

	public String getUserPassword() {
//workaround for older DTP and the constants class not present
//		return getProperty(IJDBCDriverDefinitionConstants.PASSWORD_PROP_ID);
		return getProperty(PASSWORD_PROP_ID);
//end workaround
	}

	public String getURL() {
//workaround for older DTP and the constants class not present
//		return getProperty(IJDBCDriverDefinitionConstants.URL_PROP_ID);
		return getProperty(URL_PROP_ID);
//end workaround
	}

	public String getDriverClass() {
//workaround for older DTP and the constants class not present
//		return getProperty(IJDBCDriverDefinitionConstants.DRIVER_CLASS_PROP_ID);
		return getProperty(DRIVER_CLASS_PROP_ID);
//end workaround
	}

	public String getDatabaseVendor() {
//workaround for older DTP and the constants class not present
//		return getProperty(IJDBCDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID);
		return getProperty(DATABASE_VENDOR_PROP_ID);
//end workaround
	}

	public String getPort() {
		return urlDataParser.getPort();
	}

	public String getServerName() {
		return urlDataParser.getHostName();
	}

	public String getDatabaseName() {
//workaround for older DTP and the constants class not present
//		return getProperty(IJDBCDriverDefinitionConstants.DATABASE_NAME_PROP_ID);
		return getProperty(DATABASE_NAME_PROP_ID);
//end workaround
	}
}
