/*
 * Copyright (c) 1997-2011 Oracle and/or its affiliates. All rights reserved.
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


public class Messages extends org.eclipse.osgi.util.NLS {
    static {
        org.eclipse.osgi.util.NLS.initializeMessages(
                "com.sun.enterprise.jst.server.sunappsrv.Messages", Messages.class);
    }
    
    public static String AdminName;
    public static String AdminPassword;
    public static String ServerPortNumber;
    public static String AdminServerPortNumber;
    public static String wizardSectionTitle;
    public static String wizardSectionDescription;
    public static String DomainName;
    public static String DomainDirectory;
    public static String UseAnonymousConnection;
    public static String keepSessions;
    public static String jarDeploy;
     
    // additional strings to workaround for issue 222688
    public static String canInstallPath;
    public static String possibleInstallExists;
    public static String downloadingServer;

    public static String register;
    public static String updateCenter;
 
    public static String pathDoesNotExist;
    public static String pathNotDirectory;
    public static String pathNotWritable;
    public static String pathNotValidDomain;
    public static String incompleteDomainSetup;
    public static String TitleWrongDomainLocation;
    public static String OKButton;

    public static String startupWarning;
    public static String noProfilersConfigured;
	public static String profilingUnsupportedInVersion;
	
	public static String serverDirectoryGone;
}
