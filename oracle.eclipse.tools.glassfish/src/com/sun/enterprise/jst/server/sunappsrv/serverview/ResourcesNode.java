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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.PropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.commands.CommandRunner;
import com.sun.enterprise.jst.server.sunappsrv.commands.ResourceDesc;

public class ResourcesNode extends TreeNode {

	IServer server = null;
	ResourcesNode[] children = null;
	boolean containerNode = false;
	ResourceDesc resDescriptor = null;

	public ResourcesNode(String name, String type, IServer server,
			ResourceDesc resDescriptor) {
		super(name, type, null);
		this.server = server;
		this.resDescriptor = resDescriptor;

		String[] childTypes = NodeTypes.getChildTypes(type);
		if (childTypes != null) {
			for (int i = 0; i < childTypes.length; i++) {
				String childtype = childTypes[i];
				ResourcesNode n = new ResourcesNode(childtype, childtype,
						server, null);
				if (NodeTypes.getChildTypes(childtype) != null) {
					n.setContainerNode();
				}
				addChild(n);
			}
		}

	}

	public void setContainerNode() {
		containerNode = true;
	}

	public boolean isContainerNode() {
		return containerNode;
	}

	public ResourceDesc getResource() {
		return resDescriptor;
	}

	public SunAppServerBehaviour getServerBehavior() {
		return (SunAppServerBehaviour) server.loadAdapter(
				SunAppServerBehaviour.class, null);
	}

	public Object[] getChildren() {
		// if a container node or a node that does shows a resource, return std
		// child
		if ((containerNode) || (resDescriptor != null)) {
			return childModules.toArray();
		}
		final ArrayList<ResourcesNode> list = new ArrayList<ResourcesNode>();
		if (this.children == null) {

			final SunAppServerBehaviour behaviour = getServerBehavior();
			try {
				if (behaviour == null) {
					this.children = list
							.toArray(new ResourcesNode[list.size()]);
					return this.children;
				}

				try {
					CommandRunner mgr = new CommandRunner(
							behaviour.getSunAppServer());
					List<ResourceDesc> resourcesList = mgr.getResources(type);

					for (ResourceDesc resource : resourcesList) {
						ResourcesNode t = new ResourcesNode(resource.getName(),
								type, server, resource);

						list.add(t);
					}

				} catch (Exception ex) {
					SunAppSrvPlugin.logMessage(
							"get GlassFish Resources is failing=", ex); //$NON-NLS-1$

				}
			} catch (Exception e) {
			}
			this.children = list.toArray(new ResourcesNode[list.size()]);
		}

		return this.children;
	}

	public void refresh() {
		this.children = null;
	}

	private Map<String, String> map = null;

	@Override
	public IPropertyDescriptor[] getPropertyDescriptors() {

		ArrayList<IPropertyDescriptor> properties = new ArrayList<IPropertyDescriptor>();
		PropertyDescriptor pd;
		try {
			if (resDescriptor != null) {
				CommandRunner mgr = new CommandRunner(getServerBehavior()
						.getSunAppServer());
				map = mgr.getResourceData(resDescriptor.getName());
				Set<String> s = map.keySet();
				for (String prop : s) {
					String realvalue = prop.substring(
							prop.lastIndexOf(".") + 1, prop.length());

					pd = new TextPropertyDescriptor(prop, realvalue);
					// pd.setCategory("GlassFish resources");
					properties.add(pd);
				}

			}
			return properties.toArray(new IPropertyDescriptor[0]);

		} catch (Exception ex) {
			SunAppSrvPlugin.logMessage(
					"get GlassFish Resources is failing=", ex); //$NON-NLS-1$

		}
		return null;

	}

	@Override
	public Object getPropertyValue(Object id) {
		if (resDescriptor == null) {
			return null;
		}
		if (map == null) {
			return null;
		}
		return map.get(id);
	}
}
