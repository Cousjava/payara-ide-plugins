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

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;
import com.sun.enterprise.jst.server.sunappsrv.commands.Utils;

@SuppressWarnings("restriction")
public class ServerViewContentProvider extends BaseContentProvider implements ITreeContentProvider{

	
	public ServerViewContentProvider() {
	}

	public Object[] getChildren(Object parentElement) {
		if( parentElement instanceof IServer ){
			IServer server = (IServer) parentElement;
			
			//only active for glassfish 3.1 server which is started!!!
			String s  = server.getRuntime().getRuntimeType().getId();
			if((s.equals("org.glassfish.jst.server.runtime.glassfish31") &&( server.getServerState()==IServer.STATE_STARTED))){
				SunAppServer ser = (SunAppServer)server.loadAdapter(SunAppServer.class, new NullProgressMonitor() );
				
				if( ser!=null ){
					try {
						if ((!ser.isLocalServer())&&(!Utils.isSecurePort(ser.getServer().getHost(), Integer.parseInt(ser.getAdminServerPort())))){
								TreeNode error = new TreeNode ("Error: the GlassFish server is remote, but not secured, Eclipse cannot access it", "",null);
								return new Object[]{error};
					    }
					}catch (Exception e) {
							TreeNode error = new TreeNode ("Error: "+e.getMessage(), "",null);
							return new Object[]{error};
					}
					TreeNode root = new TreeNode("GlassFish Management", "GlassFish Management", null);
					//Applications Node
					DeployedApplicationsNode apps = new DeployedApplicationsNode(server );
					//Resources Node
					DeployedWebServicesNode ws = new DeployedWebServicesNode(server );

					ResourcesNode rs = new ResourcesNode("Resources",NodeTypes.RESOURCES,server, null);
					rs.setContainerNode();
					root.addChild(apps);
					root.addChild(rs);
					root.addChild(ws);
					return new Object[]{root};
				}
			}
		}if( parentElement instanceof TreeNode ){
			TreeNode root = (TreeNode )parentElement;
			return root.getChildren();
		}
		return null;
	}

	public Object[] getElements(Object parentElement) {
		return getChildren(parentElement);
	}
	
	
	public Object getParent(Object element) {
		if( element instanceof DeployedApplicationsNode){
			return ((DeployedApplicationsNode)element).getServer();
		}else if( element instanceof ApplicationNode ){
			return ((ApplicationNode)element).getParent();
		}else if( element instanceof TreeNode ){
			TreeNode m  = (TreeNode)element;
			return m.getParent();
		}	
		return null;
	}

	public boolean hasChildren(Object element) {
		if( element instanceof IServer ){
			return true;
		}else if( element instanceof DeployedApplicationsNode ){
			return true;
		}else if( element instanceof ApplicationNode ){
			return true;
		}else if( element instanceof TreeNode ){
			TreeNode m  = (TreeNode)element;
			return m.getChildren().length>0 ;
		}
		return false;
	}

	

	
}
