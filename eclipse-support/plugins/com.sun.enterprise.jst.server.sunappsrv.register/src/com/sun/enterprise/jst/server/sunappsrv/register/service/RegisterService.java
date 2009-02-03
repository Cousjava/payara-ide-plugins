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

	public static String getPreludeLocation() {
		String property = System.getProperty("gf3location");
		String glassfishLoc = null;
		if (property != null) {
			glassfishLoc = property;
		} else {
			// Get the eclipse installation location and from it V2
			// installation directory.
			glassfishLoc = new Path(Platform.getInstallLocation().getURL().getFile()).toPortableString()
					+ "/glassfishv3prelude";

			Activator.logMessage("glassfishLoc =" + glassfishLoc, null,IStatus.INFO);
		}
		return glassfishLoc + File.separator + "glassfish" + File.separator + "lib" + File.separator + "registration"
				+ File.separator + "servicetag-registry.xml";

	}

	/**
	 * Validates the given SDN credentials and Register the product *
	 * 
	 * @param username
	 * @param passwd
	 * @param proxyHost
	 * @param proxyPort
	 * @throws ClassNotFoundException
	 * @throws NoSuchMethodException
	 * @throws IllegalAccessException
	 * @throws InstantiationException
	 * @throws InvocationTargetException
	 * @throws RegistrationException
	 * @throws UnknownHostException
	 * @throws ConnectException
	 */
	public static void validateAccountAndRegister(String username, String passwd, String proxyHost, int proxyPort)
			throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InstantiationException,
			InvocationTargetException, RegistrationException, UnknownHostException, ConnectException {

		File regPath = new File(getPreludeLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
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
	 * @param proxyHost
	 * @param proxyPort
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
			String companyname, String proxyHost, int proxyPort) throws RegistrationException, ConnectException,
			UnknownHostException, NoSuchMethodException, ClassNotFoundException, InstantiationException,
			IllegalAccessException, InvocationTargetException {

		File regPath = new File(getPreludeLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
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
		map.put("company", companyname);
		SOAccount soAccount = new SOAccount(map);

		/* Create the Account */
		rs.createRegistrationAccount(soAccount);

		/* Register the product */
		rs.register(soAccount);

		rs.setRegistrationReminder(RegistrationReminder.DONT_ASK_FOR_REGISTRATION);

	}
	
	public static void remindLater(String proxyHost, int proxyPort) throws RegistrationException {
		File regPath = new File(getPreludeLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);

		rs.setRegistrationReminder(RegistrationReminder.REMIND_LATER);
	}
	
	public static void skipRegister(String proxyHost, int proxyPort) throws RegistrationException {
		File regPath = new File(getPreludeLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);

		rs.setRegistrationReminder(RegistrationReminder.DONT_ASK_FOR_REGISTRATION);
	}

	public static boolean isRegistered(String proxyHost, int proxyPort) throws RegistrationException {
		File regPath = new File(getPreludeLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);

		return rs.getRegistrationStatus().equals(RegistrationStatus.REGISTERED);
	}

	public static RegistrationReminder getReminder(String proxyHost, int proxyPort) throws RegistrationException {
		File regPath = new File(getPreludeLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);
		return rs.getRegistrationReminder();
	}

	public static List getCountries(String proxyHost, int proxyPort) throws RegistrationException {
		File regPath = new File(getPreludeLocation());
		SysnetRegistrationService rs = null;
		if (proxyHost == null || proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
			rs = new SysnetRegistrationService(regPath);
		else
			rs = new SysnetRegistrationService(regPath, proxyHost, proxyPort);
		return rs.getAvailableCountries(Locale.getDefault());
	}
}
