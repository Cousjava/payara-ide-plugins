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

package com.sun.enterprise.jst.server.sunappsrv.sailfin;

import java.io.ByteArrayInputStream;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jst.server.core.FacetUtil;
import org.eclipse.wst.common.componentcore.ComponentCore;
import org.eclipse.wst.common.componentcore.datamodel.properties.IFacetDataModelProperties;
import org.eclipse.wst.common.componentcore.datamodel.properties.IFacetProjectCreationDataModelProperties;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;
import org.eclipse.wst.common.frameworks.datamodel.AbstractDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.project.facet.core.runtime.IRuntime;

public class SipXmlCreate extends AbstractDataModelOperation {

    private String version;

    public SipXmlCreate(IDataModel model, String version /* 1.1 */) {
        super(model);
        this.version = version;
    }

    @Override
    public IStatus execute(IProgressMonitor monitor, IAdaptable info)
            throws ExecutionException {
        IVirtualComponent comp = ComponentCore.createComponent(getProject());
        createSipXmlContent(getSipXmlFile(comp));
        return Status.OK_STATUS;
    }

    private static IFile getSipXmlFile(IVirtualComponent comp) {
        IPath sipXmlPath = comp.getRootFolder().getUnderlyingFolder()
                .getProjectRelativePath().append("WEB-INF").append("sip.xml"); //$NON-NLS-1$ //$NON-NLS-2$
        return comp.getProject().getFile(sipXmlPath);
    }

    private void createSipXmlContent(IFile sipXmlFile) {
        ByteArrayInputStream is = null;
        try {

            String moduleName = model
                    .getStringProperty(IFacetDataModelProperties.FACET_PROJECT_NAME);
            is = new ByteArrayInputStream(getDefautSipXml(moduleName)
                    .getBytes());

            sipXmlFile.create(is, false, null);

        } catch (Exception e) {

        } finally {
            try {
                if (is != null)
                    is.close();
            } catch (Exception e) {
                // do nothing
            }
        }

    }

    private String getDefautSipXml(String contextRoot) {
        return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + //$NON-NLS-1$
                "<sip-app xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" //$NON-NLS-1$
        		+ "  xmlns=\"http://www.jcp.org/xml/ns/sipservlet\"\n"  //$NON-NLS-1$
                + "  xsi:schemaLocation=\"http://www.jcp.org/xml/ns/sipservlet http://www." //$NON-NLS-1$
                + "jcp.org/xml/ns/sipservlet/sip-app_" //$NON-NLS-1$
                + version.replace(".", "_") + ".xsd\"\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                "  version=\"" + version + "\">\n" + //$NON-NLS-1$ //$NON-NLS-2$
                "</sip-app>\n"; //$NON-NLS-1$
    }

    public IStatus redo(IProgressMonitor monitor, IAdaptable info)
            throws ExecutionException {
        return null;
    }

    public IStatus undo(IProgressMonitor monitor, IAdaptable info)
            throws ExecutionException {
        return null;
    }

    public org.eclipse.wst.server.core.IRuntime getRuntime() {
        IRuntime runtime = (IRuntime) model
                .getProperty(IFacetProjectCreationDataModelProperties.FACET_RUNTIME);
        if (runtime != null) {
            return FacetUtil.getRuntime(runtime);
        }
        return null;
    }

    private IProject getProject() {
        String projectName = model.getProperty(
                IFacetDataModelProperties.FACET_PROJECT_NAME).toString();
        if (projectName != null) {
            return ResourcesPlugin.getWorkspace().getRoot().getProject(
                    projectName);
        }
        return null;
    }

}
