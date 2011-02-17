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
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.commands.WSDesc;

/**
 * A deployed web service node in the server view
 * 
 * @author Ludovic Champenois
 *
 */
public class WebServiceNode extends TreeNode{

	DeployedWebServicesNode parent;
	IServer server = null;
	TreeNode[] modules = null;
	WSDesc app = null;
	public WebServiceNode(DeployedWebServicesNode root, IServer server, WSDesc app) {
		super(app.getName(), null, root);
		this.server = server;
		this.app = app;
	}
	
	public IServer getServer(){
		return this.server;
	}
	
	public WSDesc getWSInfo(){
		return this.app;
	}
	@Override
	public IPropertyDescriptor[] getPropertyDescriptors() {
        ArrayList< IPropertyDescriptor > properties = new ArrayList< IPropertyDescriptor >();
        PropertyDescriptor pd;


                pd = new TextPropertyDescriptor( "testurl", "Test URL" );
                properties.add( pd );
                pd = new TextPropertyDescriptor( "name", "name" );
                properties.add( pd );        
                pd = new TextPropertyDescriptor( "wsdlurl", "WSDL URL" );
                properties.add( pd );        
        

        return properties.toArray( new IPropertyDescriptor[ 0 ] );
	}
	@Override
	public Object getPropertyValue(Object id) {
	       if ( id.equals( "testurl" ))
               return app.getTestURL();
	       if ( id.equals( "name" ))
                   return app.getName();
	       if ( id.equals( "wsdlurl" ))
               return app.getWsdlUrl();

     

		
		return null;
	}   	
}
