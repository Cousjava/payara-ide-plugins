package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.lang.reflect.Field;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntimeTargetHandler;
import org.eclipse.wst.server.core.IRuntime;

public class GlassFishServerRuntimeTargetHandler extends GenericServerRuntimeTargetHandler {

    /* (non-Javadoc)
     * @see ClasspathRuntimeTargetHandler#resolveClasspathContainer(IRuntime)
     */
    public IClasspathEntry[] resolveClasspathContainer(IRuntime runtime) {
        IClasspathEntry[] ent = super.resolveClasspathContainer(runtime);
        return processEntries(ent);
    }

    public IClasspathEntry[] processEntries(IClasspathEntry[] entries) {
        try {
            File jarFile = new File(GlassFishServerRuntimeTargetHandler.class.getProtectionDomain().getCodeSource().getLocation().toURI());

            for (int i = 0; i < entries.length; i++) {
                if (entries[i].getExtraAttributes().length == 0) {
                    ClasspathEntry cpe = (ClasspathEntry) entries[i];
                    IClasspathAttribute[] newa = new IClasspathAttribute[1];
                    newa[0] = JavaCore.newClasspathAttribute(IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME, "jar:file:" + jarFile.getCanonicalPath() + "!/docs/api");
                    try {
                        Field f = cpe.getClass().getDeclaredField("extraAttributes");
                        f.setAccessible(true);
                        try {
                            f.set(cpe, newa);
                            SunAppSrvPlugin.logMessage("correct!!!set" + entries[i].getExtraAttributes().length + entries[i].getPath());
                        } catch (IllegalArgumentException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        } catch (IllegalAccessException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    } catch (SecurityException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    } catch (NoSuchFieldException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }

            }
        } catch (Exception e1) {
            // TODO Auto-generated catch block
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
        return processEntries(ent);
    }
}