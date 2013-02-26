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

import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.PropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;

/**
 * A deployed app node in the server view
 * 
 * @author Ludovic Champenois
 *
 */
public class ApplicationNode extends TreeNode{

	DeployedApplicationsNode parent;
	GlassfishGenericServer server = null;
	TreeNode[] modules = null;
	AppDesc app = null;
	public ApplicationNode(DeployedApplicationsNode root, GlassfishGenericServer server, AppDesc app) {
		super(app.getName(), null, root);
		this.server = server;
		this.app = app;
	}
	
	public GlassfishGenericServer getServer(){
		return this.server;
	}
	
	public AppDesc getApplicationInfo(){
		return this.app;
	}
	@Override
	public IPropertyDescriptor[] getPropertyDescriptors() {
        ArrayList< IPropertyDescriptor > properties = new ArrayList< IPropertyDescriptor >();
        PropertyDescriptor pd;


                pd = new TextPropertyDescriptor( "contextroot", "context root" );
                pd.setCategory( "GlassFish Applications" );
                properties.add( pd );
                pd = new TextPropertyDescriptor( "name", "name" );
                pd.setCategory( "GlassFish Applications" );
                properties.add( pd );        
                pd = new TextPropertyDescriptor( "path", "path" );
                pd.setCategory( "GlassFish Applications" );
                properties.add( pd );        
                pd = new TextPropertyDescriptor( "engine", "engine" );
                pd.setCategory( "GlassFish Applications" );
                properties.add( pd );        

        return properties.toArray( new IPropertyDescriptor[ 0 ] );
	}
	@Override
	public Object getPropertyValue(Object id) {
	       if ( id.equals( "contextroot" ))
               return app.getContextRoot();
	       if ( id.equals( "name" ))
                   return app.getName();
	       if ( id.equals( "path" ))
               return app.getPath();
	       if ( id.equals( "engine" ))
               return app.getType();
     

		
		return null;
	}   	
}
