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


import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;

import com.sun.enterprise.jst.server.sunappsrv.actions.AppServerContextAction;

public class ServerViewLabelProvider extends LabelProvider implements ITableFontProvider {

	static Image resourceImg=AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/resources.gif").createImage();
	static Image serverImg=AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/sunappsrv.gif").createImage();
	static Image earImage=AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/ear.gif").createImage();
	static Image EJBImage=AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/ejb_module.gif").createImage();
	static Image WARImage = AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/web_module.gif").createImage();
	static Image WSImage=AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/webservice.png").createImage();
	
	@Override
	public String getText(Object element) {
	//	if( element instanceof DeployedApplicationsNode ){
	//		return "Applications label provider";
	//	}else if( element instanceof ResourcesNode ){
	//		return "resources label provider";
	//	}else
			if( element instanceof TreeNode ){
			TreeNode module = (TreeNode)element;
			String name = module.getName() ;
			if( name.endsWith("/") && !name.equals("/") ){
				name = name.substring(0,name.length()-1) ;
			}
			return name ;
		}
		return null;
	}

	public Image getImage(Object element) {
		 if( element instanceof ApplicationNode ){
			 ApplicationNode module = (ApplicationNode)element;
			if( module.getApplicationInfo().getType().equals("web"))
				return WARImage;
			if( module.getApplicationInfo().getType().equals("ejb"))
				return EJBImage;
			if( module.getApplicationInfo().getType().equals("ear"))
				return earImage;
		}else if (element instanceof ResourcesNode){
			ResourcesNode rn = (ResourcesNode) element;
			if (rn.getResource()==null){
				return resourceImg;
			} else {
				return serverImg;
			}
		}else if (element instanceof DeployedWebServicesNode){
			return WSImage;
		}else if (element instanceof WebServiceNode){
			return WSImage;
		}
			return serverImg ; 
		
	}

	@Override
	public Font getFont(Object arg0, int arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	
}
