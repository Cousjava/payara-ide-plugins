package com.sun.enterprise.jst.server.sunappsrv.configurator;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
    private static final String BUNDLE_NAME = "com.sun.enterprise.jst.server.sunappsrv.configurator.messages"; //$NON-NLS-1$

    public static String PROBLEM_GETTING_DERBY_JAR_LOCATION;
    public static String CONFIGURATION_OF_PORTS_FAILED_BECAUSE_OF;
    public static String EXCEPTION_OCCURRED;
    public static String CONFIGURING_SAMPLE_DERBY_DATABASE_ENCOUNTERED_A_PROBLEM;
    public static String SAMPLE_JAVADB_DATABASE_DESCRIPTION;
    public static String SAMPLE_JAVADB_DATABASE;
	public static String UNZIPPING_DERBY_SAMPLES_ENCOUNTERED_A_PROBLEM_0;
	public static String MISSING_DOMAIN_XML_PATH;
    
    public static String CreatingGlassfishv21ServerConfiguration;
    public static String CreatingGlassfishvv3PreludeConfiguration;
    public static String CreatingDemoDerbyDatabase;
    public static String UnzippingDemoDerbyDatabase;

	public static String CreatingServerConfigurationsProblem;

	public static String CreatingGlassfishServerInstances;
	public static String ErrorInStartupConfig;
	public static String GettingGlassfishV21LocationProblem;
	public static String BundledGlassFishV21;
	public static String CreatingGlassFishV21Domain;
	public static String CreatingRuntime;

	public static String ConfigurationOfPortsFailed;
	public static String FileCopyingFailed;
	public static String DomainDestinationDirectoryExists;
	public static String DomainDestinationDirectoryContainsSpace;
	public static String Bundled;

	static {
        // initialize resource bundle
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }





    private Messages() {
    }
}
