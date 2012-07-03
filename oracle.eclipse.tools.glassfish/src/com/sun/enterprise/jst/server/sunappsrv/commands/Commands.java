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


package com.sun.enterprise.jst.server.sunappsrv.commands;


import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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

            String appsList = null;
            try {
                String tmp = list.getMainAttributes().getValue("children"); // NOI18N
                if (null != tmp) {
                    appsList = tmp;
                    appsList = URLDecoder.decode(tmp, "UTF-8"); // NOI18N
                }
            } catch (UnsupportedEncodingException ex) {
                Logger.getLogger("glassfish").log(Level.WARNING, "Could not URL decode with UTF-8"); //NOI18N
            } catch (IllegalArgumentException iae) {
                // ignore this for now
            }
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

            String[] resources = resourceList.split("[,;]"); // NOI18N
            for(String r : resources) {
                if(skipResource(r)) {
                    continue;
                }

                // get container attributes
                Attributes resourceAttr = list.getAttributes(r);
                if(resourceAttr != null) {
                    String name = null;
                    String tmp = null;
                    try {
                        tmp = resourceAttr.getValue("message"); // NOI18N
                        if (null != tmp) {
                            name = URLDecoder.decode(tmp , "UTF-8"); // NOI18N
                        }

                        if (null == name || name.length() < 1)  {
                            name = URLDecoder.decode(r.trim(), "UTF-8"); // NOI18N
                        }

                    } catch (UnsupportedEncodingException uee) {
                        Logger.getLogger("glassfish").log(Level.INFO, "", uee); // NOI18N
                    }

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
                    Logger.getLogger("glassfish").log(Level.FINE, "No resource attributes returned for {0}", r); // NOI18N
                }
            }

            return true;
        }

        private boolean skipResource(String r) {
            return r.equals("Nothing to list."); //NOI18N
        }
    };

    private static void appendLibraries(StringBuilder cmd, File[] libraries) {
        cmd.append(ServerCommand.PARAM_SEPARATOR).append("libraries="); // NOI18N
        boolean firstOne = true;
        for (File f : libraries) {
            if (!firstOne) {
                cmd.append(",");
            }
            cmd.append(f.getPath()); // NOI18N
            firstOne = false;
        }
    }

    /**
     * Command to deploy a directory
     */
    public static final class DeployCommand extends ServerCommand {

        private final boolean isDirDeploy;
        private final File path;

        public DeployCommand(final File path, final String name, final String contextRoot, final String preserveSessions, final Map<String,String> properties, File[] libraries) {
            super("deploy"); // NOI18N

            this.isDirDeploy = path.isDirectory();
            this.path = path;
            
            StringBuilder cmd = new StringBuilder(128);
            cmd.append("DEFAULT="); // NOI18N
            cmd.append(path.getAbsolutePath());
            if(name != null && name.length() > 0) {
                cmd.append(PARAM_SEPARATOR).append("name="); // NOI18N
                cmd.append(sanitizeName(name));
            }
            if(contextRoot != null && contextRoot.length() > 0) {
                cmd.append(PARAM_SEPARATOR).append("contextroot="); // NOI18N
                cmd.append(contextRoot);
            }
            if (libraries.length > 0) {
                appendLibraries(cmd, libraries);
            }
            cmd.append(PARAM_SEPARATOR).append("force=true"); // NOI18N
            if (preserveSessions!=null) {
  //          	properties.put(preserveSessions, "true");
                if (preserveSessions.equals("keepstate")) {
                   cmd.append(PARAM_SEPARATOR).append(preserveSessions+"=true"); // NOI18N
                } else {
            	   properties.put("keepSessions", "true");
                }
            }
            addProperties(cmd,properties);
            query = cmd.toString();
        }

        @Override
        public String getContentType() {
            return isDirDeploy ? null : "application/zip"; // NOI18N
        }

        @Override
        public boolean getDoOutput() {
            return !isDirDeploy;
        }

        @Override
        public InputStream getInputStream() {
                if (isDirDeploy) {
                    return null;
                } else {
                    try {
                        return new FileInputStream(path);
                    } catch (FileNotFoundException fnfe) {
                        Logger.getLogger("glassfish").log(Level.INFO, path.getPath(), fnfe); // NOI18N
                        return null;
                    }
            }
        }

        @Override
        public String getRequestMethod() {
            return isDirDeploy ? super.getRequestMethod() : "POST"; // NOI18N
        }

        @Override
        public String getInputName() {
            return path.getName();
        }

        @Override
        public String getLastModified() {
            return Long.toString(path.lastModified());
        }

        private void addProperties(StringBuilder cmd, Map<String,String> properties) {
            if (null != properties && properties.size() > 0) {
                cmd.append(ServerCommand.PARAM_SEPARATOR).append("properties="); // NOI18N
                int i = 0;
                for (Entry<String,String> e : properties.entrySet()) {
                    String k = e.getKey();
                    String v = e.getValue();
                    if (i > 0) {
                        cmd.append(":"); // NOI18N
                    }
                    cmd.append(k).append("=").append(v);
                }
            }
        }
    }
//
//    private static void addProperties(StringBuilder cmd, String preserveSessions, boolean resourcesChanged) {
//        if(Boolean.TRUE.equals(preserveSessions) || !resourcesChanged) {
//            cmd.append(ServerCommand.PARAM_SEPARATOR).append("properties="); // NOI18N
//        }
//        if(preserveSessions!=null) {
//            cmd.append(preserveSessions+"=true");  // NOI18N
//            if (!resourcesChanged) {
//                cmd.append(":"); // NOI18N
//            }
//        }
//        if (!resourcesChanged) {
//            cmd.append("preserveAppScopedResources=true"); // NOI18N
//        }
//    }

    /**
     * Command to undeploy a deployed application.
     */
    public static final class UndeployCommand extends ServerCommand {

        public UndeployCommand(final String name) {
            super("undeploy"); // NOI18N
            query = "name=" +sanitizeName(name); // NOI18N
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
                cmd.append("cascade=true"); // NOI18N
                cmd.append(PARAM_SEPARATOR);
            }
            cmd.append(cmdPropertyName);
            cmd.append('=');
            cmd.append(name);
            query = cmd.toString();
        }
    }
    
    public static String sanitizeName(String name) {
        if (null == name || name.matches("[\\p{L}\\p{N}_][\\p{L}\\p{N}\\-_./;#:]*")) {
            return name;
        }
        // the string is bad...
        return "_" + name.replaceAll("[^\\p{L}\\p{N}\\-_./;#:]", "_");
    }
    
    public static final class AddResourcesCommand extends ServerCommand {

        public AddResourcesCommand(String sunResourcesXmlPath) {
            super("add-resources"); // NOI18N
            query = "xml_file_name=" + sunResourcesXmlPath; // NOI18N
        }
        
    }    
}

