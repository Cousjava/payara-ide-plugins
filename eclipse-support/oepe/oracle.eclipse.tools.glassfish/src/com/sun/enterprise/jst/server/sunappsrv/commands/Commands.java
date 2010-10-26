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


import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.logging.Level;
import java.util.logging.Logger;



/**
 * Abstraction of commands for V3 server administration
 *
 * @author Peter Williams
 */
public class Commands {

    // ------------------------------------------------------------------------
    // Specific server commands.
    // ------------------------------------------------------------------------
    
    /**
     * Command to start a server domain
     */
    public static final ServerCommand START = new ServerCommand("start-domain") { // NOI18N
    };
    
    /**
     * Command to stop a server domain
     */
    public static final ServerCommand STOP = new ServerCommand("stop-domain") { // NOI18N
    };
    
    /**
     * Command to list applications current deployed on the server.
     */
    public static final class ListAppsCommand extends ServerCommand {
        
        private final String container;
        private Manifest list;
        private Map<String, List<AppDesc>> appMap; 
        
        public ListAppsCommand() {
            this(null);
        }
        
        public ListAppsCommand(final String container) {
            super("list-applications"); // NOI18N
            this.container = container;
        }
        
        public String [] getContainers() {
            String [] result = null;
            if(appMap != null && appMap.size() > 0) {
                Set<String> containers = appMap.keySet();
                result = containers.toArray(new String[containers.size()]);
            }
            return result != null ? result : new String[0];
        }
        
        public Map<String, List<AppDesc>> getApplicationMap() {
            // !PW Can still modify sublist... is there a better structure?
            if(appMap != null) {
                return Collections.unmodifiableMap(appMap);
            } else {
                return Collections.emptyMap();
            }
        }
        
        @Override
        public void readManifest(Manifest manifest) throws IOException {
            list = manifest;
        }
        
        @Override
        public boolean processResponse() {
            if(list == null) {
                return false;
            }
            
            String appsList = list.getMainAttributes().getValue("children"); // NOI18N
            if(appsList == null || appsList.length() == 0) {
                // no applications deployed...
                return true;
            }

            String [] apps = appsList.split(";"); // NOI18N
            for(String appKey: apps) {
                if("null".equals(appKey)) {
                    Logger.getLogger("glassfish").log(Level.WARNING, 
                            "list-applications contains an invalid result.  " +
                            "Check server log for possible exceptions.");
                    continue;
                }
                
                Attributes appAttrs = list.getAttributes(appKey);
                if(appAttrs == null) {
                    continue;
                }
                
                String engine = getPreferredEngine(appAttrs.getValue("nb-engine_value"));
                
                String name = appAttrs.getValue("nb-name_value");
                if(name == null || name.length() == 0) {
                    Logger.getLogger("glassfish").log(Level.FINE, "Skipping application with no name..."); // FIXME better log message.
                    continue;
                }
                
                String path = appAttrs.getValue("nb-location_value");
                if(path.startsWith("file:")) {
                    path = path.substring(5);
                }
                
                String contextRoot = appAttrs.getValue("nb-context-root_value");
                if(contextRoot == null) {
                    contextRoot = name;
                }
                if(contextRoot.startsWith("/")) {
                    contextRoot = contextRoot.substring(1);
                }

                // Add app to proper list in result map
                if(appMap == null) {
                    appMap = new HashMap<String, List<AppDesc>>();
                }
                List<AppDesc> appList = appMap.get(engine);
                if(appList == null) {
                    appList = new ArrayList<AppDesc>();
                    appMap.put(engine, appList);
                }
                
                appList.add(new AppDesc(name, path, contextRoot));
            }

            return true;
        }

        // XXX temporary patch to handle engine descriptions like <web, ejb>
        // until we have better display semantics for such things.
        // XXX bias order of list for JavaONE demos.
        private static final List<String> engineBias = 
                Arrays.asList(new String [] { "jruby", "web", "ejb" });
        
        private String getPreferredEngine(String engineList) {
            String [] engines = engineList.split(",");
            String engine = null;
            int bias = -1;
            for(int i = 0; i < engines.length; i++) {
                if(!skipContainer(engines[i])) {
                    engines[i] = engines[i].trim();
                    int newBias = engineBias.indexOf(engines[i]);
                    if(newBias >= 0 && (bias == -1 || newBias < bias)) {
                        bias = newBias;
                    }
                    if(engine == null) {
                        engine = engines[i];
                    }
                }
            }
            if(bias != -1) {
                engine = engineBias.get(bias);
            } else if(engine == null) {
                engine = "unknown";
            }
            return engine;
        }

        /**
         * For skipping containers we don't care about.
         * 
         * @param container
         * @return
         */
        private boolean skipContainer(String currentContainer) {
            return container != null ? !container.equals(currentContainer) :
                "security_ContractProvider".equals(currentContainer);
        }
        
    };
    
    /**
     * Command to list applications current deployed on the server.
     * Uses list-components
     */
    public static final class ListComponentsCommand extends ServerCommand {

        private final String container;
        private Manifest list;
        private Map<String, List<String>> appMap;

        public ListComponentsCommand() {
            this(null);
        }

        public ListComponentsCommand(final String container) {
            super("list-components"); // NOI18N
            this.container = container;
        }

        public String[] getContainers() {
            String[] result = null;
            if(appMap != null && appMap.size() > 0) {
                Set<String> containers = appMap.keySet();
                result = containers.toArray(new String[containers.size()]);
            }
            return result != null ? result : new String[0];
        }

        public Map<String, List<String>> getApplicationMap() {
            // !PW Can still modify sublist... is there a better structure?
            if(appMap != null) {
                return Collections.unmodifiableMap(appMap);
            } else {
                return Collections.emptyMap();
            }
        }

        @Override
        public void readManifest(Manifest manifest) throws IOException {
            list = manifest;
        }

        @Override
        public boolean processResponse() {
            if(list == null) {
                return false;
            }

            String appsList = list.getMainAttributes().getValue("children"); // NOI18N
            if(appsList == null || appsList.length() == 0) {
                // no applications deployed...
                return true;
            }

            String[] apps = appsList.split(";"); // NOI18N
            for(String appKey : apps) {
                if("null".equals(appKey)) { // NOI18N
                    Logger.getLogger("glassfish").log(Level.WARNING, "list-components contains an invalid result.  " + "Check server log for possible exceptions."); // NOI18N
                    continue;
                }

                String[] keys = appKey.split("[<>]");
                String name = keys[0];
                if(name == null || name.length() == 0) {
                    Logger.getLogger("glassfish").log(Level.FINE, "Skipping application with no name..."); // NOI18N  FIXME better log message.
                    continue;
                }
                String engine = getPreferredEngine(keys[1]); // NOI18N

                // Add app to proper list in result map
                if(appMap == null) {
                    appMap = new HashMap<String, List<String>>();
                }
                List<String> appList = appMap.get(engine);
                if(appList == null) {
                    appList = new ArrayList<String>();
                    appMap.put(engine, appList);
                }
                appList.add(name);
            }

            return true;
        }
        // XXX temporary patch to handle engine descriptions like <web, ejb>
        // until we have better display semantics for such things.
        // XXX bias order of list for JavaONE demos.
        private static final List<String> engineBias =
                Arrays.asList(new String[]{"ear", "jruby", "web", "ejb", "appclient", "connector"}); // NOI18N

        private String getPreferredEngine(String engineList) {
            String[] engines = engineList.split(",");  // NOI18N
            String engine = null;
            int bias = -1;
            for(int i = 0; i < engines.length; i++) {
                if(!skipContainer(engines[i])) {
                    engines[i] = engines[i].trim();
                    int newBias = engineBias.indexOf(engines[i]);
                    if(newBias >= 0 && (bias == -1 || newBias < bias)) {
                        bias = newBias;
                    }
                    if(engine == null) {
                        engine = engines[i];
                    }
                }
            }
            if(bias != -1) {
                engine = engineBias.get(bias);
            } else if(engine == null) {
                engine = "unknown"; // NOI18N
            }
            return engine;
        }

        /**
         * For skipping containers we don't care about.
         *
         * @param container
         * @return
         */
        private boolean skipContainer(String currentContainer) {
            return container != null ? !container.equals(currentContainer) :
                    "security_ContractProvider".equals(currentContainer); // NOI18N
        }
    };
    
    /**
     * Command to list resources of various types currently available on the server.
     */
    public static final class ListResourcesCommand extends ServerCommand {

        private final String cmdSuffix;
        private Manifest list;
        private List<ResourceDesc> resList;

        public ListResourcesCommand(String resourceCmdSuffix) {
            super("list-" + resourceCmdSuffix + "s"); // NOI18N

            cmdSuffix = resourceCmdSuffix;
        }
        
        public List<ResourceDesc> getResourceList() {
            if(resList != null) {
                return Collections.unmodifiableList(resList);
            } else {
                return Collections.emptyList();
            }
        }
        
        @Override
        public void readManifest(Manifest manifest) throws IOException {
            list = manifest;
        }
        
        @Override
        public boolean processResponse() {
            if(list == null) {
                return false;
            }
            
            String resourceList = list.getMainAttributes().getValue("children"); // NOI18N
            if(resourceList == null || resourceList.length() == 0) {
                // no resources running...
                return true;
            }

            String [] resources = resourceList.split("[,;]"); // NOI18N
            for(String r: resources) {
                if(skipResource(r)) {
                    continue;
                }

                // get container attributes
                Attributes resourceAttr = list.getAttributes(r);
                if(resourceAttr != null) {
                    String name = resourceAttr.getValue("message"); // NOI18N

                    if (null == name || name.length() < 1)  {
                        name = r.trim();
                    }
                    
                    if(name != null && name.length() > 0) {
                        if(resList == null) {
                            resList = new ArrayList<ResourceDesc>();
                        }

                        resList.add(new ResourceDesc(name, cmdSuffix));
                    }
                } else {
                    Logger.getLogger("glassfish").log(Level.FINE, "No resource attributes returned for " + r);
                }
            }
            
            return true;
        }

        private boolean skipResource(String r) {
            return false;
        }
    
    };

 
    /**
     * Command to deploy a directory
     */
    public static final class DeployCommand extends ServerCommand {
        
        public DeployCommand(final String path, final String name, final String contextRoot, final Boolean preserveSessions) {
            super("deploy"); // NOI18N
            
            StringBuilder cmd = new StringBuilder(128);
            cmd.append("path="); // NOI18N
            cmd.append(path);
            if(name != null && name.length() > 0) {
                cmd.append(PARAM_SEPARATOR + "name="); // NOI18N
                cmd.append(name);
            }
            if(contextRoot != null && contextRoot.length() > 0) {
                cmd.append(PARAM_SEPARATOR + "contextroot="); // NOI18N
                cmd.append(contextRoot);
            }
            cmd.append(PARAM_SEPARATOR + "force=true");
            addKeepSessions(cmd,preserveSessions);
            query = cmd.toString();
        }
        
    }
    
    /**
     * Command to redeploy a directory deployed app that is already deployed.
     */
    public static final class RedeployCommand extends ServerCommand {
        
        public RedeployCommand(final String name, final String contextRoot, final Boolean preserveSessions) {
            super("redeploy"); // NOI18N
            
            StringBuilder cmd = new StringBuilder(128);
            cmd.append("name="); // NOI18N
            cmd.append(name);
            if(contextRoot != null && contextRoot.length() > 0) {
                cmd.append(PARAM_SEPARATOR + "contextroot="); // NOI18N
                cmd.append(contextRoot);
            }
            addKeepSessions(cmd,preserveSessions);
            query = cmd.toString();
        }
        
    }
    
    private static void addKeepSessions(StringBuilder cmd, Boolean preserveSessions) {
        if (Boolean.TRUE.equals(preserveSessions)) {

            cmd.append(ServerCommand.PARAM_SEPARATOR + "property="); // NOI18N
            cmd.append("keepSessions=true");
        }        
    }
    
    /**
     * Command to undeploy a deployed application.
     */
    public static final class UndeployCommand extends ServerCommand {
        
        public UndeployCommand(final String name) {
            super("undeploy"); // NOI18N
            query = "name=" + name;
        }
        
    }
    
    /**
     * Command to unregister a resource.
     */
    public static final class UnregisterCommand extends ServerCommand {
        
        public UnregisterCommand(final String name, final String resourceCmdSuffix,
                final String cmdPropertyName, final boolean cascade) {
            super("delete-" + resourceCmdSuffix); // NOI18N

            StringBuilder cmd = new StringBuilder(128);
            if(cascade) {
                cmd.append("cascade=true");
                cmd.append(PARAM_SEPARATOR);
            }
            cmd.append(cmdPropertyName);
            cmd.append('=');
            cmd.append(name);
            query = cmd.toString();
        }
        
    }
    
    /**
     * Command to get version information from the server.
     */
    public static final class VersionCommand extends ServerCommand {

        private Manifest info;
        
        public VersionCommand() {
            super("version"); // NOI18N
        }

        @Override
        public void readManifest(Manifest manifest) throws IOException {
            info = manifest;
        }

        @Override
        public boolean processResponse() {
            return true;
        }
        
    }
    
    /**
     * Command to get version information from the server.
     */
    public static final class LocationCommand extends ServerCommand {

        private Manifest info;
        private String installRoot;
        private String domainRoot;
        
        public LocationCommand() {
            super("__locations"); // NOI18N
        }
        
        public String getInstallRoot() {
            return installRoot;
        }

        public String getDomainRoot() {
            return domainRoot;
        }

        @Override
        public void readManifest(Manifest manifest) throws IOException {
            info = manifest;
        }

        @Override
        public boolean processResponse() {
            if(info == null) {
                return false;
            }
            
            Attributes mainAttrs = info.getMainAttributes();
            if(mainAttrs != null) {
                installRoot = mainAttrs.getValue("Base-Root_value");
                domainRoot = mainAttrs.getValue("Domain-Root_value");
            }
            
            return true;
        }
    }
    
    public static final class AddResourcesCommand extends ServerCommand {

        public AddResourcesCommand(String sunResourcesXmlPath) {
            super("add-resources"); // NOI18N
            query = "xml_file_name=" + sunResourcesXmlPath; // NOI18N
        }
        
    }
}

