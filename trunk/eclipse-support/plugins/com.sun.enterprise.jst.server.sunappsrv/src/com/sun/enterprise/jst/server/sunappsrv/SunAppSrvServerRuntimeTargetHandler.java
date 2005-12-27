// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt or 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt.  If applicable, 
 * add the following below this CDDL HEADER, with the 
 * fields enclosed by brackets "[]" replaced with your 
 * own identifying information: Portions Copyright [yyyy] 
 * [name of copyright owner]
 */
// </editor-fold>



package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntimeTargetHandler;
import org.eclipse.jst.server.generic.core.internal.ServerTypeDefinitionUtil;
import org.eclipse.jst.server.generic.servertype.definition.ArchiveType;
import org.eclipse.jst.server.generic.servertype.definition.Classpath;
import org.eclipse.jst.server.generic.servertype.definition.ServerRuntime;
import org.eclipse.wst.server.core.IRuntime;

public class SunAppSrvServerRuntimeTargetHandler extends GenericServerRuntimeTargetHandler {
    
    String cachedArchiveString=null;
    IClasspathEntry[] cachedClasspath=null;
    
    
    public IClasspathEntry[] resolveClasspathContainer(IRuntime runtime,String id){
        return getServerClassPathEntry(runtime);
    }
    
    public IClasspathEntry[] getServerClassPathEntry(IRuntime runtime) {
        ServerRuntime serverDefinition = ServerTypeDefinitionUtil.getServerTypeDefinition(runtime);
        String ref = serverDefinition.getProject().getClasspathReference();
        Classpath cp = serverDefinition.getClasspath(ref);
        List archives = cp.getArchive();
        
        IClasspathEntry[] savedClasspath=getCachedClasspathFor(serverDefinition, archives);
        if(savedClasspath!=null)
            return savedClasspath;
        
        Iterator archiveIter = archives.iterator();
        ArrayList entryList = new ArrayList();
        while (archiveIter.hasNext()) {
            ArchiveType archive = (ArchiveType) archiveIter.next();
            String item = serverDefinition.getResolver().resolveProperties(archive.getPath());
            Path path=new Path(item);
            File file=path.toFile();
            if(file.isDirectory()){
                File[] list=file.listFiles();
                for(int i=0; i<list.length; i++){
                    if(!list[i].isDirectory()){
                        Path p=new Path(list[i].getAbsolutePath());
                        IClasspathEntry entry = JavaCore.newLibraryEntry(p,null,null );
                        entryList.add(entry);
                    }
                }
                
            } else{
                IClasspathEntry entry = JavaCore.newLibraryEntry(path,null,null );
                entryList.add(entry);
            }
        }
        
        IClasspathEntry[] classpath=(IClasspathEntry[])entryList.toArray(new IClasspathEntry[entryList.size()]);
        setCachedClasspath(classpath);
        
        return classpath;
    }
    
    private IClasspathEntry[] getCachedClasspathFor(ServerRuntime serverDefinition, List archives) {
        
        
        StringBuffer buffer=new StringBuffer();
        Iterator archiveIter = archives.iterator();
        while (archiveIter.hasNext()) {
            ArchiveType archive = (ArchiveType) archiveIter.next();
            String item = serverDefinition.getResolver().resolveProperties(archive.getPath());
            buffer.append(item);
            buffer.append(File.pathSeparatorChar);
        }
        
        String archiveString=buffer.toString();
        
        if(cachedArchiveString != null && cachedArchiveString.equals(archiveString))
            return cachedClasspath;
        
        cachedClasspath=null;
        cachedArchiveString=archiveString;
        return null;
    }
    
    private void setCachedClasspath(IClasspathEntry[] classpath) {
        cachedClasspath=classpath;
    }
    
}
