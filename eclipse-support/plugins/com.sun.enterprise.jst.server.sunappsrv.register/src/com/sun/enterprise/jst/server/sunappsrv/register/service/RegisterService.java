package com.sun.enterprise.jst.server.sunappsrv.register.service;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.ConnectException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;

import com.sun.enterprise.registration.RegistrationAccount;
import com.sun.enterprise.registration.RegistrationException;
import com.sun.enterprise.registration.impl.SOAccount;
import com.sun.enterprise.registration.impl.SysnetRegistrationService;

@SuppressWarnings("unchecked")
public class RegisterService {

	/**
	 * Validates the given SDN credentials and Register the product *
	 * 
	 * @param username
	 * @param passwd
	 * @param localRegistryPath
	 *            = glassfish installHome + File.separator + "glassfish" +
	 *            File.separator + "lib" + File.separator + "registration" +
	 *            File.separator + "servicetag-registry.xml";
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
	public static void validateAccountAndRegister(String username, String passwd, String localRegistryPath,
			String proxyHost, int proxyPort) throws ClassNotFoundException, NoSuchMethodException,
			IllegalAccessException, InstantiationException, InvocationTargetException, RegistrationException,
			UnknownHostException, ConnectException {

		File regPath = new File(localRegistryPath);
		SysnetRegistrationService rs = null;
		if (proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
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
	 * @param localRegistryPath
	 *            = glassfish installHome + File.separator + "glassfish" +
	 *            File.separator + "lib" + File.separator + "registration" +
	 *            File.separator + "servicetag-registry.xml";
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
			String companyname, String localRegistryPath, String proxyHost, int proxyPort)
			throws RegistrationException, ConnectException, UnknownHostException, NoSuchMethodException,
			ClassNotFoundException, InstantiationException, IllegalAccessException, InvocationTargetException {

		File regPath = new File(localRegistryPath);
		SysnetRegistrationService rs = null;
		if (proxyHost.trim() == null || proxyHost.length() == 0 || proxyPort == 0)
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

	}

}
