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
import java.io.StringWriter;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;



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

    public static final ServerCommand RESTART = new ServerCommand("restart-domain") { // NOI18N
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

            String[] resources = resourceList.split("[,;]"); // NOI18N
            for(String r : resources) {
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
                    Logger.getLogger("glassfish").log(Level.FINE, "No resource attributes returned for {0}", r); // NOI18N
                }
            }

            return true;
        }

        private boolean skipResource(String r) {
            return r.equals("Nothing to list"); //NOI18N
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

        public DeployCommand(final File path, final String name, final String contextRoot, final Boolean preserveSessions, final Map<String,String> properties, File[] libraries) {
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
            if (Boolean.TRUE.equals(preserveSessions)) {
            	properties.put("keepSessions", "true");
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

    /**
     * Command to redeploy a directory deployed app that is already deployed.
     */
    public static final class RedeployCommand extends ServerCommand {

        public RedeployCommand(final String name, final String contextRoot, final Boolean preserveSessions, 
                final File[] libraries, final boolean resourcesChanged) {
            super("redeploy"); // NOI18N

            StringBuilder cmd = new StringBuilder(128);
            cmd.append("name="); // NOI18N
            cmd.append(sanitizeName(name));
            if(contextRoot != null && contextRoot.length() > 0) {
                cmd.append(PARAM_SEPARATOR).append("contextroot="); // NOI18N
                cmd.append(contextRoot);
            }
            if (libraries.length > 0) {
                appendLibraries(cmd, libraries);
            }
            addProperties(cmd, preserveSessions, resourcesChanged);
            query = cmd.toString();
        }
    }

    private static void addProperties(StringBuilder cmd, Boolean preserveSessions, boolean resourcesChanged) {
        if(Boolean.TRUE.equals(preserveSessions) || !resourcesChanged) {
            cmd.append(ServerCommand.PARAM_SEPARATOR).append("properties="); // NOI18N
        }
        if(Boolean.TRUE.equals(preserveSessions)) {
            cmd.append("keepSessions=true");  // NOI18N
            if (!resourcesChanged) {
                cmd.append(":"); // NOI18N
            }
        }
        if (!resourcesChanged) {
            cmd.append("preserveAppScopedResources=true"); // NOI18N
        }
    }

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
     * Command to enable a deployed application.
     */
    public static final class EnableCommand extends ServerCommand {

        public EnableCommand(final String name) {
            super("enable"); // NOI18N
            query = "DEFAULT=" + sanitizeName(name); // NOI18N
        }
    }

    /**
     * Command to disable a deployed application.
     */
    public static final class DisableCommand extends ServerCommand {

        public DisableCommand(final String name) {
            super("disable"); // NOI18N
            query = "DEFAULT=" + sanitizeName(name); // NOI18N
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

    /**
     * Command to get version information from the server.
     */
    public static final class VersionCommand extends ServerCommand {

        private Manifest info;
        private String version;
        public VersionCommand() {
            super("version"); // NOI18N
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
                version = mainAttrs.getValue("message");
            }
            
            return true;
        }
        public String getVersion(){
        	return version;
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
                installRoot = mainAttrs.getValue("Base-Root_value");  // NOI18N
                domainRoot = mainAttrs.getValue("Domain-Root_value");  // NOI18N
            }

            return true;
        }
    }
    
    /*
     * Command to get log data from the server
     */
    public static final class FetchLogData extends ServerCommand {
        private String lines = "";
        private String nextURL = "";
        
        public FetchLogData(String query) {
            super("view-log");
            this.query = query;
        }
        
        public String getLines() {
            return lines;
        }

        public String getNextQuery() {
            return nextURL;
        }

        @Override
        public boolean acceptsGzip() {
            return true;
        }
        
        @Override
        public boolean readResponse(InputStream in, HttpURLConnection hconn) {
            StringWriter sw = new StringWriter();
            try {
                InputStream cooked = in;
                String ce = hconn.getContentEncoding();
                if (null != ce && ce.contains("gzip")) {
                    cooked = new GZIPInputStream(in);
                }
                java.io.InputStreamReader isr = new java.io.InputStreamReader(cooked);
                java.io.BufferedReader br = new java.io.BufferedReader(isr);
                while (br.ready()) {
                    sw.write(br.readLine());
                    sw.write("\n");
                }
            } catch (IOException ex) {
                ex.printStackTrace();
            } finally {
                try {
                    sw.close();
                } catch (IOException ex) {
                    
                }
            }
            lines = sw.toString();
            nextURL = hconn.getHeaderField("X-Text-Append-Next");
            int delim = nextURL.lastIndexOf("?");
            if (-1 != delim) {
                nextURL = nextURL.substring(delim+1);
            }
            return -1 == delim ? false : true;
        }
        
        @Override
        public String getSrc() {
            return "/management/domain/";
        }
    }

    static class ListWebservicesCommand extends ServerCommand {

        private Manifest manifest;
        private List<String> wsList;

        public ListWebservicesCommand() {
            super("__list-webservices"); // NOI18N
        }

        public List<String> getWebserviceList() {
            // !PW Can still modify sublist... is there a better structure?
            if(wsList != null) {
                return Collections.unmodifiableList(wsList);
            } else {
                return Collections.emptyList();
            }
        }

        @Override
        public void readManifest(Manifest manifest) throws IOException {
            this.manifest = manifest;
        }

        @Override
        public boolean processResponse() {
            if(manifest == null) {
                return false;
            }
            
            Map <String, String> filter = new HashMap<String, String>();

            Iterator<String> keyIterator = manifest.getEntries().keySet().iterator();
            while (keyIterator.hasNext()) {
                String k = keyIterator.next();
                if (!k.contains("address:/")) // NOI18N
                    continue;
                if (k.contains("address:/wsat-wsat")) // NOI18N
                    continue;
                if (k.contains("address:/__wstx-services")) // NOI18N
                    continue;
                String a = k.replaceFirst(".* address:/", "").replaceFirst("\\. .*", ""); // NOI18N
                if (filter.containsKey(a))
                    continue;
                filter.put(a,a);
                if(wsList == null) {
                    wsList = new ArrayList<String>();
                }
                wsList.add(a);
            }

            return true;
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

