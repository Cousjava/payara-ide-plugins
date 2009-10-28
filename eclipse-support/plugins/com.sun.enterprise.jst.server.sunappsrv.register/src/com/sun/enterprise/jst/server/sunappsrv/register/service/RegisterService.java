/*DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.

The contents of this file are subject to the terms of either the GNU
General Public License Version 2 only ("GPL") or the Common Development
and Distribution License("CDDL") (collectively, the "License").  You
may not use this file except in compliance with the License. You can obtain
a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
language governing permissions and limitations under the License.

When distributing the software, include this License Header Notice in each
file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
Sun designates this particular file as subject to the "Classpath" exception
as provided by Sun in the GPL Version 2 section of the License file that
accompanied this code.  If applicable, add the following below the License
Header, with the fields enclosed by brackets [] replaced by your own
identifying information: "Portions Copyrighted [year]
[name of copyright owner]"

Contributor(s):

If you wish your version of this file to be governed by only the CDDL or
only the GPL Version 2, indicate your decision by adding "[Contributor]
elects to include this software in this distribution under the [CDDL or GPL
Version 2] license."  If you don't indicate a single choice of license, a
recipient has the option to distribute your version of this file under
either the CDDL, the GPL Version 2 or to extend the choice of license to
its licensees as provided above.  However, if you add GPL Version 2 code
and therefore, elected the GPL Version 2 license, then the option applies
only if the new code is made subject to such option by the copyright
holder.
 */
package com.sun.enterprise.jst.server.sunappsrv.register.service;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.ConnectException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;
import com.sun.enterprise.registration.RegistrationAccount;
import com.sun.enterprise.registration.RegistrationException;
import com.sun.enterprise.registration.RegistrationService.RegistrationReminder;
import com.sun.enterprise.registration.RegistrationService.RegistrationStatus;
import com.sun.enterprise.registration.impl.SOAccount;
import com.sun.enterprise.registration.impl.SysnetRegistrationService;

@SuppressWarnings("unchecked")
public class RegisterService {

	private static String proxyHost = null;
	private static int proxyPort = 0;

	private static String getv3Location() {
		String property = System.getProperty("gf3location"); //$NON-NLS-1$
		String glassfishLoc = null;
		if (property != null) {
			glassfishLoc = property;
		} else {
			// Get the eclipse installation location and from it V2
			// installation directory.
			glassfishLoc = new Path(Platform.getInstallLocation().getURL().getFile()).toPortableString()
					+ "glassfishv3-prelude"; //$NON-NLS-1$
			File loc = new File(glassfishLoc);
            if (!loc.exists() || !loc.isDirectory()) {
    			glassfishLoc = new Path(Platform.getInstallLocation().getURL().getFile()).toPortableString()
				+ "glassfishv3"; //$NON-NLS-1$
            }

			Activator.logMessage("glassfishLoc =" + glassfishLoc, null, IStatus.INFO); //$NON-NLS-1$
		}
		return glassfishLoc;
		
	}
	public static String getv3PreludeServiceTagRegistryLocation() {
		String glassfishLoc = getv3Location();
		return glassfishLoc + File.separator + "glassfish" + File.separator + "lib" + File.separator + "registration" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				+ File.separator + "servicetag-registry.xml"; //$NON-NLS-1$

	}

	/**
	 * Validates the given SDN credentials and Register the product *
	 * 
	 * @param username
	 * @param passwd
	 * @throws ClassNotFoundException
	 * @throws NoSuchMethodException
	 * @throws IllegalAccessException
	 * @throws InstantiationException
	 * @throws InvocationTargetException
	 * @throws RegistrationException
	 * @throws UnknownHostException
	 * @throws ConnectException
	 */
	public static void validateAccountAndRegister(String username, String passwd) throws ClassNotFoundException,
			NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException,
			RegistrationException, UnknownHostException, ConnectException {

		File regPath = new File(getv3PreludeServiceTagRegistryLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);

		/* validate account */
		HashMap map = new HashMap();
		map.put(RegistrationAccount.USERID, username);
		map.put(RegistrationAccount.PASSWORD, passwd);
		SOAccount account = new SOAccount(map);

		/* Validate the Account */
		rs.isRegistrationAccountValid(account);

		/* Register product with account */
		rs.register(account);

		rs.setRegistrationReminder(RegistrationReminder.DONT_ASK_FOR_REGISTRATION);
	}

	/**
	 * Creates the SDN Account *
	 * 
	 * @param email
	 * @param passwd
	 * @param country
	 * @param firstname
	 * @param lastname
	 * @param companyname
	 * @throws RegistrationException
	 * @throws ConnectException
	 * @throws UnknownHostException
	 * @throws NoSuchMethodException
	 * @throws ClassNotFoundException
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 * @throws InvocationTargetException
	 */
	public static void createSDNAccount(String email, String passwd, String country, String firstname, String lastname,
			String companyname) throws RegistrationException, ConnectException, UnknownHostException,
			NoSuchMethodException, ClassNotFoundException, InstantiationException, IllegalAccessException,
			InvocationTargetException {

		File regPath = new File(getv3PreludeServiceTagRegistryLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);
		Map map = new HashMap();
		map.put(RegistrationAccount.EMAIL, email);
		map.put(RegistrationAccount.PASSWORD, passwd);
		map.put(RegistrationAccount.USERID, email);
		map.put(RegistrationAccount.FIRSTNAME, firstname);
		map.put(RegistrationAccount.LASTNAME, lastname);
		map.put(RegistrationAccount.COUNTRY, country);
		map.put("company", companyname); //$NON-NLS-1$
		SOAccount soAccount = new SOAccount(map);

		/* Create the Account */
		rs.createRegistrationAccount(soAccount);

		/* Register the product */
		rs.register(soAccount);

		rs.setRegistrationReminder(RegistrationReminder.DONT_ASK_FOR_REGISTRATION);

	}

	public static void remindLater() throws RegistrationException {
		File regPath = new File(getv3PreludeServiceTagRegistryLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);

		rs.setRegistrationReminder(RegistrationReminder.REMIND_LATER);
	}

	public static void skipRegister() throws RegistrationException {
		File regPath = new File(getv3PreludeServiceTagRegistryLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);

		rs.setRegistrationReminder(RegistrationReminder.DONT_ASK_FOR_REGISTRATION);
	}

	public static boolean isRegistered() throws RegistrationException {
		File regPath = new File(getv3PreludeServiceTagRegistryLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);

		return rs.getRegistrationStatus().equals(RegistrationStatus.REGISTERED);
	}

	public static RegistrationReminder getReminder() throws RegistrationException {
		File regPath = new File(getv3PreludeServiceTagRegistryLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);
		return rs.getRegistrationReminder();
	}

	public static List getCountries() throws RegistrationException {
		File regPath = new File(getv3PreludeServiceTagRegistryLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);
		return rs.getAvailableCountries(Locale.getDefault());
	}

	public static void setProxy(String host, int port) {
		if ("".equals(host)) {
			proxyHost = null;
		} else
			proxyHost = host;
		proxyPort = port;
	}
}
