package com.sun.enterprise.jst.server.sunappsrv.register;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "com.sun.enterprise.jst.server.sunappsrv.register.messages"; //$NON-NLS-1$

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}
	
	public static String ERROR_GETTING_REGISTRATION_STATUS;
	public static String EXCEPTION_OCCURRED;
	public static String CONFIGURING_GLASSFISH_V2_1_ENCOUNTERED_A_PROBLEM_0;
	public static String CONFIGURING_GLASSFISH_V2_1_INSTALLATION;
	public static String INITIALIZING_SPLASH_HANDLER;
	public static String CLICK_FINISH_TO_REGISTER_YOUR_ACCOUNT_AND_REGISTER_GLASSFISH;
	public static String PLEASE_INSERT_YOUR_COUNTRY;
	public static String PLEASE_INSERT_YOUR_COMPANY_NAME;
	public static String PLEASE_INSERT_YOUR_LAST_NAME;
	public static String PLEASE_INSERT_YOUR_FIRST_NAME;
	public static String PASSWORDS_DON_T_MATCH;
	public static String PLEASE_INSERT_PASSWORD;
	public static String PLEASE_INSERT_EMAIL_ADRESS;
	public static String CREATING_AN_SDN_ACCOUNT_FAILED;
	public static String ERROR_GETTING_COUNTRIES_LIST;
	public static String COUNTRY;
	public static String COMPANY_NAME;
	public static String LAST_NAME;
	public static String FIRST_NAME;
	public static String RETYPE_PASSWORD;
	public static String PASSWORD;
	public static String EMAIL_ADDRESS;
	public static String PLEASE_ENTER_YOUR_PERSONAL_INFORMATION;
	public static String PERSONAL_INFORMATION;
	public static String PLEASE_INSERT_USERNAME_AND_PASSWORD;
	public static String CLICK_FINISH_TO_REGISTER_GLASSFISH_SERVER;
	public static String SUPPORT_AND_TRAINING_OFFERING;
	public static String SCREENCASTS_AND_TUTORIALS;
	public static String PATCH_INFORMATION_AND_BUG_UPDATES;
	public static String WHY_REGISTER_GAIN_CONVENIENT_ACCESS_TO_BENEFITS_SUCH_AS;
	public static String I_ALREADY_HAVE_A_SUN_ONLINE_ACCOUNT;
	public static String I_DON_T_HAVE_A_SUN_ONLINE_ACCOUNT_SIGN_ME_UP;
	public static String REGISTER_ACCOUNT;
	public static String CHOOSE_REGISTRATION_METHOD;
	public static String DIRECTORY_0_DOESN_T_CONTAIN_JAVA_SDK_INSTALLATION;
	public static String PLEASE_SELECT_JAVA_SDK_INSTALLATION_LOCATION_FOR_GLASSFISH_V2_1;
	public static String SKIP_REGISTRATION;
	public static String USER_NAME;

	private Messages() {
	}
}
