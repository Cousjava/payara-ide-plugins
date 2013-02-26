/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Oracle
 */
package com.sun.enterprise.jst.server.sunappsrv.serverview;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.ui.internal.viewers.BaseContentProvider;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class ServerViewContentProvider extends BaseContentProvider implements ITreeContentProvider {

    public ServerViewContentProvider() {
    	return;
    }

    @Override
    public Object[] getChildren(Object parentElement) {
        if (parentElement instanceof IServer) {
            IServer server = (IServer) parentElement;

            //only active for glassfish 3.1.x server which is started!!!
            boolean is31x = SunAppSrvPlugin.is31OrAbove(server.getRuntime());
            if ((is31x && (server.getServerState() == IServer.STATE_STARTED))) {
                GlassfishGenericServer ser = (GlassfishGenericServer) server.loadAdapter(GlassfishGenericServer.class, new NullProgressMonitor());

                if (ser != null) {
                    TreeNode root = new TreeNode("GlassFish Management", "GlassFish Management", null);
                    //Applications Node
                    DeployedApplicationsNode apps = new DeployedApplicationsNode(ser);
                    //Resources Node
                    DeployedWebServicesNode ws = new DeployedWebServicesNode(ser);

                    ResourcesNode rs = new ResourcesNode("Resources", NodeTypes.RESOURCES, ser, null);
                    rs.setContainerNode();
                    root.addChild(apps);
                    root.addChild(rs);
                    root.addChild(ws);
                    return new Object[]{root};
                }
            }
        }
        if (parentElement instanceof TreeNode) {
            TreeNode root = (TreeNode) parentElement;
            return root.getChildren();
        }
        return null;
    }

    @Override
    public Object[] getElements(Object parentElement) {
        return getChildren(parentElement);
    }

    @Override
    public Object getParent(Object element) {
        if (element instanceof DeployedApplicationsNode) {
            return ((DeployedApplicationsNode) element).getServer();
        } else if (element instanceof ApplicationNode) {
            return ((ApplicationNode) element).getParent();
        } else if (element instanceof TreeNode) {
            TreeNode m = (TreeNode) element;
            return m.getParent();
        }
        return null;
    }

    @Override
    public boolean hasChildren(Object element) {
        if (element instanceof IServer) {
            return true;
        } else if (element instanceof DeployedApplicationsNode) {
            return true;
        } else if (element instanceof ApplicationNode) {
            return true;
        } else if (element instanceof TreeNode) {
            TreeNode m = (TreeNode) element;
            return m.getChildren().length > 0;
        }
        return false;
    }
}
