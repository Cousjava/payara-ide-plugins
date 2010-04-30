package com.sun.enterprise.jst.server.sunappsrv.derby;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
    private static final String BUNDLE_NAME = "com.sun.enterprise.jst.server.sunappsrv.derby.messages"; //$NON-NLS-1$

    public static String PROBLEM_GETTING_DERBY_JAR_LOCATION;
    public static String CONFIGURATION_OF_PORTS_FAILED_BECAUSE_OF;
    public static String EXCEPTION_OCCURRED;
    public static String CONFIGURING_SAMPLE_DERBY_DATABASE_ENCOUNTERED_A_PROBLEM;
    public static String SAMPLE_JAVADB_DATABASE_DESCRIPTION;
    public static String SAMPLE_JAVADB_DATABASE;
	public static String UNZIPPING_DERBY_SAMPLES_ENCOUNTERED_A_PROBLEM_0;

    public static String CreatingDemoDerbyDatabase;
    public static String UnzippingDemoDerbyDatabase;



	static {
        // initialize resource bundle
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }





    private Messages() {
    }
}
