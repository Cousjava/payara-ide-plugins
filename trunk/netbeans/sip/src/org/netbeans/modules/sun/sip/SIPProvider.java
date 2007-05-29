/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 * 
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 * 
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.sun.sip;

import java.awt.Component;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Set;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.libraries.Library;
import org.netbeans.api.project.libraries.LibraryManager;
import org.netbeans.modules.j2ee.dd.api.web.DDProvider;
import org.netbeans.modules.j2ee.dd.api.web.Servlet;
import org.netbeans.modules.j2ee.dd.api.web.ServletMapping;
import org.netbeans.modules.j2ee.dd.api.web.WebApp;
import org.netbeans.modules.web.api.webmodule.WebModule;
import org.netbeans.modules.web.spi.webmodule.FrameworkConfigurationPanel;
import org.netbeans.modules.web.spi.webmodule.WebFrameworkProvider;
import org.netbeans.spi.java.project.classpath.ProjectClassPathExtender;
import org.openide.DialogDescriptor;
import org.openide.ErrorManager;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

/**
 *
 * @author Ludovic Champenois
 *
 */
public class SIPProvider extends WebFrameworkProvider {
    
    protected String libname = "siplibrary";
    /** Creates a new instance of LibraryProvider */
    public SIPProvider() {
        super(
                NbBundle.getMessage(SIPProvider.class, "sip_Name"),// NOI18N
                NbBundle.getMessage(SIPProvider.class, "sip_Description"));//NOI18N
    }
    public SIPProvider(String n, String d) {
        super(n,d);
    }    
    public Set extend(WebModule wm) {
        final  FileObject documentBase = wm.getDocumentBase();
        Project project = FileOwnerQuery.getOwner(documentBase);
        
        try {
            FileObject dd = wm.getDeploymentDescriptor();
            WebApp ddRoot = DDProvider.getDefault().getDDRootCopy(dd);
            if (ddRoot != null){
                Library bpLibrary = LibraryManager.getDefault().getLibrary(libname);
                if (bpLibrary != null) {
                    ProjectClassPathExtender cpExtender = (ProjectClassPathExtender) project.getLookup().lookup(ProjectClassPathExtender.class);
                    if (cpExtender != null) {
                        try {
                            cpExtender.addLibrary(bpLibrary);
                            
                        } catch (IOException ioe) {
                            
                        }
                    } else {
                    }
                }
                
                
//                // Enter servlet into the deployment descriptor
//                try{
//                    Servlet servlet = (Servlet)ddRoot.createBean("Servlet"); //NOI18N
//                    servlet.setServletName("sip-invoker"); //NOI18N
//                    servlet.setDisplayName("SIP Servlet"); //NOI18N
//                    servlet.setServletClass("com.sun.dddd.SIPServlet"); //NOI18N
//                    ddRoot.addServlet(servlet);
//                    
////                  servlet.setLoadOnStartup(new BigInteger("1"));//NOI18N
//                    
//                    
//                    ServletMapping mapping = (ServletMapping)ddRoot.createBean("ServletMapping"); //NOI18N
//                    mapping.setServletName("sip-invoker");//NOI18N
//                    mapping.setUrlPattern("/sip/*");//NOI18N
//                    
//                    ddRoot.addServletMapping(mapping);
//                    
//                    
//                    ddRoot.write(dd);
//                } catch (Exception e){
//                    e.printStackTrace();
//                }
                try {
                    FileSystem fs = wm.getWebInf().getFileSystem();
                    fs.runAtomicAction(new CreateSIPConfig(wm));
                    
                } catch (FileNotFoundException exc) {
                    ErrorManager.getDefault().notify(exc);
                    return null;
                } catch (IOException exc) {
                    ErrorManager.getDefault().notify(exc);
                    return null;
                }
                
                
            }
            
        } catch (IOException exc) {
            exc.printStackTrace() ;
            return null;
        }
        return null;
    }
    
    
    public java.io.File[] getConfigurationFiles(org.netbeans.modules.web.api.webmodule.WebModule wm) {
        
        return null;
    }
    
    public FrameworkConfigurationPanel getConfigurationPanel(WebModule wm) {
        
        
        return new FrameworkConfigurationPanel() {
            public void enableComponents(boolean enable) {
            }
            
            public Component getComponent() {
                return new JPanelSIP();
            }
            
            public HelpCtx getHelp() {
                return HelpCtx.DEFAULT_HELP;
            }
            
            public void readSettings(Object settings) {
            }
            
            public void storeSettings(Object settings) {
            }
            
            public boolean isValid() {
                return true;
            }
            
            public void addChangeListener(ChangeListener l) {
            }
            
            public void removeChangeListener(ChangeListener l) {
            }
            
            
        };
    }
    
    public boolean isInWebModule(org.netbeans.modules.web.api.webmodule.WebModule wm) {
        FileObject dd = wm.getDeploymentDescriptor();
        
        if (dd == null) {
            return false;
        }
        try {
            WebApp webApp = DDProvider.getDefault().getDDRoot(dd);
            
            boolean ret=
              webApp
                    .findBeanByName("Servlet", "ServletClass", "uk.ltd.getahead.dwr.DWRServlet")!=null; //NOI18N;
            System.out.println("SIP REturn ="+ret);
            return ret;
        } catch (java.io.IOException e) {
            return false;
        }
    }
    
    public String getServletPath(FileObject file){
        String url = null;
        if (file == null) return url;
        
        WebModule wm = WebModule.getWebModule(file);
        if (wm != null){
            url = FileUtil.getRelativePath(wm.getDocumentBase(), file);
            if (url.charAt(0)!='/')
                url = "/" + url;
            
        }
        return url;
    }
    
    
    
    
    private class  CreateSIPConfig implements FileSystem.AtomicAction{
        WebModule wm;
        public CreateSIPConfig(WebModule wm){
            this.wm = wm;
        }
        
        
        public void run() throws IOException {
            if (canCreateNewFile(wm.getWebInf(), "sip.xml")) { //NOI18N
                File f = new File(FileUtil.toFile(wm.getWebInf()), "sip.xml");
                ConfigProvider cp = new ConfigProvider();
                cp.save(f);
            }
            
            
        }
        
    }
    
    private boolean canCreateNewFile(FileObject parent, String name){
        File f = new File(FileUtil.toFile(parent), name);
        boolean create = true;
        if (f.exists()){
            DialogDescriptor dialog = new DialogDescriptor(
                    NbBundle.getMessage(SIPProvider.class, "MSG_OverwriteFile", f.getAbsolutePath()),
                    NbBundle.getMessage(SIPProvider.class, "TTL_OverwriteFile"),
                    true, DialogDescriptor.YES_NO_OPTION, DialogDescriptor.NO_OPTION, null);
            java.awt.Dialog d = org.openide.DialogDisplayer.getDefault().createDialog(dialog);
            d.setVisible(true);
            create = (dialog.getValue() == org.openide.DialogDescriptor.NO_OPTION);
        }
        return create;
    }
    
    
    
    
}





