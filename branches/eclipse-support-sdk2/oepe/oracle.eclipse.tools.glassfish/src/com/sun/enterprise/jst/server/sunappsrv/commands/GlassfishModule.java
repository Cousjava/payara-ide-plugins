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


package com.sun.enterprise.jst.server.sunappsrv.commands;

public class GlassfishModule {
    
    // Attribute keys for InstanceProperties map
    public static final String URL_ATTR = "url"; // NOI18N
    public static final String INSTALL_FOLDER_ATTR = "installfolder"; // NOI18N
    public static final String GLASSFISH_FOLDER_ATTR = "homefolder"; // NOI18N
    public static final String DISPLAY_NAME_ATTR = "displayName"; // NOI18N
    public static final String USERNAME_ATTR = "username"; // NOI18N
    public static final String PASSWORD_ATTR = "password"; // NOI18N
    public static final String ADMINPORT_ATTR = "adminPort"; // NOI18N
    public static final String HTTPPORT_ATTR = "httpportnumber"; // NOI18N
    public static final String HOSTNAME_ATTR = "host"; // NOI18N
    public static final String JRUBY_HOME = "jruby.home"; // NOI18N
    public static final String DOMAINS_FOLDER_ATTR = "domainsfolder"; // NOI18N
    public static final String DOMAIN_NAME_ATTR = "domainname";
    public static final String HTTP_MONITOR_FLAG = "httpMonitorOn";
    public static final String DRIVER_DEPLOY_FLAG = "driverDeployOn";
    
    public static final String DEBUG_PORT = "debugPort"; // NOI18N
    public static final String JVM_MODE = "jvmMode"; // NOI18N
    public static final String NORMAL_MODE = "normalMode"; // NOI18N
    public static final String DEBUG_MODE = "debugMode"; // NOI18N
    public static final String PROFILE_MODE = "profileMode"; // NOI18N
    
    public static final String COMET_FLAG = "v3.grizzly.cometSupport"; // NOI18N
    
    // Contract provider constants (identify the different containers in V3)
    public static final String WEB_CONTAINER = "web"; // NOI18N
    public static final String JRUBY_CONTAINER = "jruby"; // NOI18N

    // Resource types
    public static final String JDBC_RESOURCE = "jdbc-resource"; // NOI18N
    public static final String JDBC_CONNECTION_POOL = "jdbc-connection-pool"; // NOI18N
    public static final String SESSION_PRESERVATION_FLAG = "preserveSessionsOn";
    /**
     * Enum for the current state of the server (stopped, running, etc.)
     */
    public static enum ServerState {
        STARTING,
        RUNNING,
        RUNNING_JVM_DEBUG,
        STOPPING,
        STOPPED,
        STOPPED_JVM_BP
    }
    
    /**
     * Enum for the current state of a server operation (e.g start, stop, deploy)
     */
    public static enum OperationState {
        RUNNING,
        COMPLETED,
        FAILED
    }
}
