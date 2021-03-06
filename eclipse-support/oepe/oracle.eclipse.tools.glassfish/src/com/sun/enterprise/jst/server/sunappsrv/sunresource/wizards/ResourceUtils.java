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


package com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jst.j2ee.project.JavaEEProjectUtilities;
import org.eclipse.wst.common.componentcore.ComponentCore;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;
import org.eclipse.wst.common.componentcore.resources.IVirtualFolder;
import org.glassfish.tools.ide.admin.Command;
import org.glassfish.tools.ide.admin.CommandGetProperty;
import org.glassfish.tools.ide.admin.CommandSetProperty;
import org.glassfish.tools.ide.admin.ResultMap;
import org.glassfish.tools.ide.admin.ResultString;
import org.glassfish.tools.ide.admin.ServerAdmin;
import org.glassfish.tools.ide.admin.TaskState;
import org.glassfish.tools.ide.data.GlassFishServer;
import org.glassfish.tools.ide.data.IdeContext;
import org.glassfish.tools.ide.server.parser.ResourcesReader;
import org.glassfish.tools.ide.server.parser.ResourcesReader.ResourceType;
import org.glassfish.tools.ide.server.parser.TreeParser;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class ResourceUtils {
	public static final String RESOURCE_FILE_TEMPLATE = "templates/sun-resources-xml-template.resource"; //$NON-NLS-1$
	public static final String RESOURCE_FILE_NAME = "sun-resources.xml"; //$NON-NLS-1$
	public static final String WEB_CONTENT = "WebContent"; //$NON-NLS-1$
	public static final String WEB_INF = "WEB-INF"; //$NON-NLS-1$
	public static final String EAR_CONTENT = "EarContent"; //$NON-NLS-1$
	public static final String EJB_CONTENT = "ejbModule"; //$NON-NLS-1$
	public static final String META_INF = "META-INF"; //$NON-NLS-1$
	
	private static final String SUN_RESOURCES_XML_HEADER = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + "<!DOCTYPE resources PUBLIC " +  //$NON-NLS-1$ //$NON-NLS-2$
            "\"-//Sun Microsystems, Inc.//DTD Application Server 9.0 Resource Definitions //EN\" " + //$NON-NLS-1$ 
            "\"http://www.sun.com/software/appserver/dtds/sun-resources_1_3.dtd\">\n" + //$NON-NLS-1$
        "<resources>\n"; //$NON-NLS-1$
    private static final String SUN_RESOURCES_XML_FOOTER = "</resources>\n"; //$NON-NLS-1$
   
    public static InputStream appendResource(IFile sunResourcesXml, String fragment) throws IOException, CoreException {
        String sunResourcesBuf = readResourceFile(sunResourcesXml);
        sunResourcesBuf = insertFragment(sunResourcesBuf, fragment);
        return new ByteArrayInputStream(sunResourcesBuf.getBytes());
    }
    
    public static String readResourceFile(IFile sunResourcesXml) throws IOException, CoreException {
        String content = null;
        if(sunResourcesXml.exists()) {
        	InputStream is = null;
        	Reader reader = null;
        	try {
        		IPath location = sunResourcesXml.getLocation();
        		if (location != null) {
        			File resFile = location.toFile();
        			long flen = resFile.length();
        			if(flen > 1000000) {
        				throw new IOException(resFile.getAbsolutePath() + " is too long to update."); //$NON-NLS-1$
        			}
        			int length = (int) (2 * flen + 32);
        			char [] buf = new char[length];
        			is = new BufferedInputStream(sunResourcesXml.getContents());
        			String encoding = sunResourcesXml.getCharset();
        			reader = new InputStreamReader(is, encoding);
        			int max = reader.read(buf);
        			if(max > 0) {
        				content = new String(buf, 0, max);
        			}
        		}
        	} finally {
        		if(is != null) {
        			try { is.close(); } catch(IOException ex) { }
        		}
        		if(reader != null) {
        			try { reader.close(); } catch(IOException ex) { }
        		}
        	}
        }
        return content;
    }
    
	public static String insertFragment(String sunResourcesBuf, String fragment) throws IOException {
        String header = SUN_RESOURCES_XML_HEADER;
        String footer = SUN_RESOURCES_XML_FOOTER;
        boolean insertNewLine = false;
        
        if(sunResourcesBuf != null) {
            int closeIndex = sunResourcesBuf.indexOf("</resources>"); //$NON-NLS-1$
            if(closeIndex == -1) {
                throw new IOException("Malformed XML"); //$NON-NLS-1$
            }
            header = sunResourcesBuf.substring(0, closeIndex);
            footer = sunResourcesBuf.substring(closeIndex);
            
            if(closeIndex > 0 && sunResourcesBuf.charAt(closeIndex-1) != '\n') { //$NON-NLS-1$
                insertNewLine = true;
            }
        }
        
        int length = header.length() + footer.length() + 2;
        if(fragment != null) {
            length += fragment.length();
        }
        
        StringBuilder builder = new StringBuilder(length);
        builder.append(header);
        
        if(insertNewLine) {
            String lineSeparator = System.getProperty("line.separator"); //$NON-NLS-1$
            builder.append(lineSeparator != null ? lineSeparator : "\n"); //$NON-NLS-1$
        }
        
        if(fragment != null) {
            builder.append(fragment);
        }
        
        builder.append(footer);
        return builder.toString();
    }
	
	public static String replaceOrRemove(String originalLine, String pattern, String value) {
		String containsPattern = ".*" + pattern + ".*"; //$NON-NLS-1$ //$NON-NLS-2$
		if ((originalLine != null) && Pattern.matches(containsPattern, originalLine)) {
			return (((value == null) || (value.length() == 0)) ? null : 
				originalLine.replaceAll(pattern, value));
		}
		return originalLine;
	}
	
	public static String getResourceLocation(IProject project){
		String setUpLocation = getProjectResourceLocation(project);
		if(setUpLocation == null) {
			if(JavaEEProjectUtilities.isDynamicWebProject(project)){
				setUpLocation = WEB_CONTENT + File.separatorChar + WEB_INF;			
			}else if (JavaEEProjectUtilities.isEARProject(project)){
				setUpLocation = EAR_CONTENT;
			}else if (JavaEEProjectUtilities.isEJBProject(project)){
				setUpLocation = EJB_CONTENT + File.separatorChar + META_INF;
			}
		} else {
			if(JavaEEProjectUtilities.isDynamicWebProject(project)){
				setUpLocation = setUpLocation + File.separatorChar + WEB_INF;			
			}else if (JavaEEProjectUtilities.isEJBProject(project)){
				setUpLocation = setUpLocation + File.separatorChar + META_INF;
			}
		}
		return setUpLocation;
	}
	
	public static String getRuntimeResourceLocation(IProject project){
		String setUpLocation = null;
		if(JavaEEProjectUtilities.isDynamicWebProject(project)){
			setUpLocation = WEB_INF;			
		}else if (JavaEEProjectUtilities.isEARProject(project)){
			setUpLocation = ""; //$NON-NLS-1$
		}else if (JavaEEProjectUtilities.isEJBProject(project)){
			setUpLocation = META_INF;
		}
		return setUpLocation;
	}
	
	private static String getProjectResourceLocation(IProject project){
		String setUpLocation = null;
		IVirtualComponent component = ComponentCore.createComponent(project);
		IVirtualFolder contentFolder = component.getRootFolder();
		IContainer resourceFolder = contentFolder.getUnderlyingFolder();
		setUpLocation = resourceFolder.getName();
		return setUpLocation;
	}	
	
	private static IFile getSunResourceIFile(IProject selectedProject) {
		String dirName = getResourceLocation(selectedProject);
		IContainer containerResource = selectedProject;
		IFolder folder = containerResource.getFolder(new Path(dirName));
		IFile file = folder.getFile(new Path(RESOURCE_FILE_NAME));
		return file;
	}
	
	public static File getSunResourceFile(IProject selectedProject) {
		File resFile = null;
		IFile sunResourcesXml = getSunResourceIFile(selectedProject);
		IPath location = sunResourcesXml.getLocation();
		if (location != null) {
			resFile = location.toFile();
		}
		return resFile;
	}
	
		
	public static List<String> getResources(IProject selectedProject, ResourceType... types){
		List<String> resources = new ArrayList<String>();
		if (selectedProject != null) {
			File xmlFile = getSunResourceFile(selectedProject);
			if (xmlFile.exists()) {
				for (ResourceType type : types) {
					ResourcesReader reader = new ResourcesReader(type);
					TreeParser.readXml(xmlFile, reader);
					resources.addAll(reader.getResourceData().keySet());
				}
			}
		} else {
			SunAppSrvPlugin.logMessage("No valid project selected");
		}
		return resources;
	}

	public static void checkUpdateServerResources(File sunResourcesXml, GlassfishGenericServer sunAppsrv) {
		Map<String, String> changedData = new HashMap<String, String>();
		
		ResourcesReader cpReader = new ResourcesReader(ResourceType.JDBC_CONNECTION_POOL);
		ResourcesReader jdbcReader = new ResourcesReader(ResourceType.JDBC_RESOURCE);
		ResourcesReader connectorPoolReader = new ResourcesReader(ResourceType.CONNECTOR_POOL);
		ResourcesReader connectorResourceReader = new ResourcesReader(ResourceType.CONNECTOR_RESOURCE);
		ResourcesReader aoReader = new ResourcesReader(ResourceType.ADMIN_OBJECT_RESOURCE);
		ResourcesReader mailReader = new ResourcesReader(ResourceType.JAVA_MAIL);

		try {
			TreeParser.readXml(sunResourcesXml, cpReader, jdbcReader, connectorPoolReader,
					connectorResourceReader, aoReader, mailReader);
		} catch (IllegalStateException ex) {
			SunAppSrvPlugin.logMessage("Exception while reading resource file : " + sunResourcesXml, ex);	//$NON-NLS-1$
		}
		Map<String, String> allRemoteData = getResourceData(sunAppsrv, null); //$NON-NLS-1$
		changedData = checkResources(cpReader, "resources.jdbc-connection-pool.", allRemoteData, changedData); //$NON-NLS-1$
		changedData = checkResources(jdbcReader, "resources.jdbc-resource.", allRemoteData, changedData); //$NON-NLS-1$
		changedData = checkResources(connectorPoolReader, "resources.connector-connection-pool.", allRemoteData, changedData); //$NON-NLS-1$
		changedData = checkResources(connectorResourceReader, "resources.connector-resource.", allRemoteData, changedData); //$NON-NLS-1$
		changedData = checkResources(aoReader, "resources.admin-object-resource.", allRemoteData, changedData); //$NON-NLS-1$
		changedData = checkResources(mailReader, "resources.mail-resource.", allRemoteData, changedData); //$NON-NLS-1$

		if (changedData.size() > 0) {
			try {
				putResourceData(sunAppsrv, changedData);
			} catch (PartialCompletionException e) {
				SunAppSrvPlugin.logMessage("Some of the resources were not updated!", e);
			}
		}
	}

	public static Map<String, String> getResourceData(GlassFishServer server, String name) {
		try {
            //GetPropertyCommand cmd;
            String query;
            if (null != name) {
                query = "resources.*."+name+".*"; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                query = "resources.*"; //$NON-NLS-1$
            }
            Command command = new CommandGetProperty(query);
            Future<ResultMap<String, String>> future = ServerAdmin.<ResultMap<String, String>>exec(server, command, new IdeContext());
            ResultMap<String, String> result = future.get(30, TimeUnit.SECONDS);
            
            if (TaskState.COMPLETED.equals(result.getState())) {
            	Map<String, String> retVal = result.getValue();
            	if (retVal.isEmpty())
                    Logger.getLogger("glassfish").log(Level.INFO, null, new IllegalStateException(query+" has no data"));  //$NON-NLS-1$
                return retVal;
            }
        } catch (InterruptedException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  //$NON-NLS-1$
        } catch (Exception ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  //$NON-NLS-1$
        }
        return new HashMap<String,String>();
    }
	
	private static Map<String, String> checkResources(ResourcesReader resourceReader, String prefix, Map<String, String> allRemoteData, Map<String, String> changedData) {
        Set<String> resources = resourceReader.getResourceData().keySet();
        for (String jndiName : resources) {
            Map<String, String> localData = resourceReader.getResourceData().get(jndiName);
            String remoteKey = prefix + jndiName + "."; //$NON-NLS-1$

            Map<String, String> remoteData = new HashMap<String, String>();
            Iterator<String> itr = allRemoteData.keySet().iterator();
            while (itr.hasNext()) {
            	String key = (String) itr.next();
            	if(key.startsWith(remoteKey)){
            		remoteData.put(key, allRemoteData.get(key));
            	}
            }
            if (remoteData.size() > 0) {
            	changedData = getChangedData(remoteData, localData, changedData, remoteKey);
            }
        }
        return changedData;
    }
	
	private static Map<String, String> getChangedData(Map<String, String> remoteData, Map<String, String> localData, Map<String, String> changedData, String resourceKey) {
        List<String> props = new ArrayList<String>();
        Iterator<String> keys = remoteData.keySet().iterator();
        Set<String> localKeySet = localData.keySet();
        while (keys.hasNext()) {
            String remoteDataKey = keys.next();
            String remoteValue = remoteData.get(remoteDataKey);
            String[] split = remoteDataKey.split(resourceKey);
            String key = split[1];
            if (key.indexOf("property.") != -1) { //$NON-NLS-1$
                props.add(key);
            }
            String localValue = (String) localData.get(key);
            if (localValue != null) {
                if ((remoteValue == null) || ((remoteValue != null) && (!localValue.equals(remoteValue)))) {
                	changedData.put(remoteDataKey, localValue);
                }
            } else {
                if (localKeySet.contains(key)) {
                    if (remoteValue != null) {
                        changedData.put(remoteDataKey, localValue);
                    }
                }
            }
        }
        keys = localData.keySet().iterator();
        while (keys.hasNext()) {
            String key = keys.next();
            if (key.indexOf("property.") != -1) { //$NON-NLS-1$
                if (!props.contains(key)) {
                    String remoteKey = resourceKey + key;
                    changedData.put(remoteKey, localData.get(key));
                }
            }
        }
        return changedData;
    }
	
	public static void putResourceData(GlassFishServer server, Map<String, String> data) throws PartialCompletionException {
		String itemsNotUpdated = null;
        Throwable lastEx = null;
        for (String k : data.keySet()) {
            String compName = k;
            String compValue = data.get(k);

            try {
            	Command command = new CommandSetProperty(compName, compValue);
            	Future<ResultString> future = 
                        ServerAdmin.<ResultString>exec(server, command, new IdeContext());
            	ResultString result = future.get(30, TimeUnit.SECONDS);
            	if (!TaskState.COMPLETED.equals(result.getState())) {
            		itemsNotUpdated = addName(compName, itemsNotUpdated);
            	}
            } catch (InterruptedException ex) {
                lastEx = ex;
                Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
                itemsNotUpdated = addName(compName, itemsNotUpdated);
            } catch (Exception ex) {
                lastEx = ex;
                Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);  // NOI18N
                itemsNotUpdated = addName(compName, itemsNotUpdated);
            }
        }
        if (null != itemsNotUpdated) {
            PartialCompletionException pce = new PartialCompletionException(itemsNotUpdated);
            if (null != lastEx) {
                pce.initCause(lastEx);
            }
            throw pce;
        }
    }
	
	
	private static String addName(final String compName, final String itemsNotUpdated) {
        String retVal = itemsNotUpdated;
        if (null != itemsNotUpdated) {
            retVal += ", "+compName;
        } else {
            retVal = compName;
        }
        return retVal;
    }
	
    public static String getUniqueResourceName(String name, List<String> resources){
		for (int i = 1;; i++) {
			String resourceName = name + "_" + i; //$NON-NLS-1$
			if (! resources.contains(resourceName)) {
				return resourceName;
			}
		}
	}
	
	public static boolean isDuplicate (String name, List<String> resources){
		boolean isDuplicate = false;
		if (resources.contains(name)) {
				isDuplicate = true;
		}
		return isDuplicate;
	}
	
	public static boolean isDuplicate (String name, ResourceType type, IProject selectedProject){
		boolean isDuplicate = false;
		List<String> resources = getResources(selectedProject, type);
		if (resources.contains(name)) {
				isDuplicate = true;
		}
		return isDuplicate;
	}
}
