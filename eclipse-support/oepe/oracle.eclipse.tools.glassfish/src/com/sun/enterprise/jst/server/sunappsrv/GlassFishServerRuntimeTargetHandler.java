/*
 * Copyright (c) 2010 Oracle and/or its affiliates. All rights reserved.
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

import java.lang.reflect.Field;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntimeTargetHandler;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import java.net.URL;

@SuppressWarnings("restriction")
public class GlassFishServerRuntimeTargetHandler extends GenericServerRuntimeTargetHandler {

    /* (non-Javadoc)
     * @see ClasspathRuntimeTargetHandler#resolveClasspathContainer(IRuntime)
     */
    public IClasspathEntry[] resolveClasspathContainer(IRuntime runtime) {
        IClasspathEntry[] ent = super.resolveClasspathContainer(runtime);
   //     SunAppSrvPlugin.logMessage("IClasspathEntry[] resolveClasspathContainer(IRuntime runtime)  " + runtime.getRuntimeType().getId(),null);
        return processEntries(runtime , ent);
    }

    public IClasspathEntry[] processEntries(IRuntime runtime ,IClasspathEntry[] entries) {
        try {
        	
          //  SunAppSrvPlugin.logMessage("URLPATH=" +GlassFishServerRuntimeTargetHandler.class.getProtectionDomain().getCodeSource().getLocation().getPath(),null);
          //  SunAppSrvPlugin.logMessage("URLPATHfile=" +GlassFishServerRuntimeTargetHandler.class.getProtectionDomain().getCodeSource().getLocation().getFile(),null);

       //     File jarFile = new File(""+GlassFishServerRuntimeTargetHandler.class.getProtectionDomain().getCodeSource().getLocation().getPath());
            String relativeDocPath="!/docs/api";
            if (runtime.getRuntimeType().getId().equals("com.sun.enterprise.jst.server.runtime.sunappsrv92")){//GlassFish v3
            	relativeDocPath="!/javaee6doc/doc";
            } else
            if (runtime.getRuntimeType().getId().equals("org.glaasfish.jst.server.runtime.glassfish31")){//GlassFish 3.1
            	relativeDocPath="!/javaee6doc/doc";
            }

           String javadocPath ="jar:file:" + GlassFishServerRuntimeTargetHandler.class.getProtectionDomain().getCodeSource().getLocation().getPath() + relativeDocPath;

           Bundle b = null;
           if (runtime.getRuntimeType().getId().equals("com.sun.enterprise.jst.server.runtime.sunappsrv92")){//GlassFish v3
        	   b = Platform.getBundle("oracle.eclipse.tools.doc.javaee6");
               if (b!=null){
            	   URL u= FileLocator.resolve(b.getEntry("/javaee6doc/doc"));
                   javadocPath = ""+u;
              }
           } else    if (runtime.getRuntimeType().getId().equals("org.glassfish.jst.server.runtime.glassfish31")){//GlassFish 3.1
        	   b = Platform.getBundle("oracle.eclipse.tools.doc.javaee6");
               if (b!=null){
            	   URL u= FileLocator.resolve(b.getEntry("/javaee6doc/doc"));
                   javadocPath = ""+u;
              }
           }
           else {
	           b = Platform.getBundle("oracle.eclipse.tools.doc.javaee5");
	           if (b!=null){
	        	   URL u= FileLocator.resolve(b.getEntry("/javaee5doc/docs/api"));
	               javadocPath = ""+u;
	          }           }

           
          //  SunAppSrvPlugin.logMessage("entry-------::::" +javadocPath,null);

            for (int i = 0; i < entries.length; i++) {

                if (entries[i].getExtraAttributes().length == 0) {
                 //   SunAppSrvPlugin.logMessage("entries[i].getExtraAttributes().length" +entries[i].getExtraAttributes().length,null);

                    ClasspathEntry cpe = (ClasspathEntry) entries[i];
                    IClasspathAttribute[] newa = new IClasspathAttribute[1];
                    if ((""+entries[i].getPath()).endsWith("grizzly-comet.jar")){ //for comet apis
                    	newa[0] = JavaCore.newClasspathAttribute(IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME, 
                                "jar:file:" + GlassFishServerRuntimeTargetHandler.class.getProtectionDomain().getCodeSource().getLocation().getPath() + "!/cometdoc");

                    }else { //for Java ee 
                        newa[0] = JavaCore.newClasspathAttribute(IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME, javadocPath);

                    }
                    try {
                        Field f = cpe.getClass().getDeclaredField("extraAttributes");
                        f.setAccessible(true);
                        try {
                            f.set(cpe, newa);
                        //    SunAppSrvPlugin.logMessage("correct!!!set" + entries[i].getExtraAttributes().length + entries[i].getPath());
                        } catch (IllegalArgumentException e) {
                            SunAppSrvPlugin.logMessage("error processEntries" ,e);
                            e.printStackTrace();
                        } catch (IllegalAccessException e) {
                            SunAppSrvPlugin.logMessage("error processEntries" ,e);
                            e.printStackTrace();
                        }
                    } catch (SecurityException e) {
                        SunAppSrvPlugin.logMessage("error processEntries" ,e);
                        e.printStackTrace();
                    } catch (NoSuchFieldException e) {
                        SunAppSrvPlugin.logMessage("error processEntries" ,e);
                        e.printStackTrace();
                    }
                }

            }
        } catch (Exception e1) {
            SunAppSrvPlugin.logMessage("error processEntries" ,e1);
            e1.printStackTrace();
        }
        return entries;

    }

    /**
     * Read the classpath entries for the serverdef.
     *
     * @param runtime
     * @param monitor
     * @return classpathEntries
     */
    public IClasspathEntry[] getDelegateClasspathEntries(IRuntime runtime, IProgressMonitor monitor) {
        IClasspathEntry[] ent = super.getDelegateClasspathEntries(runtime, monitor);
    //    SunAppSrvPlugin.logMessage("IClasspathEntry[] resolveClasspathContainer(IRuntime runtime)  " + runtime.getRuntimeType().getId(),null);

        return processEntries(runtime , ent);
    }
}
