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


package com.sun.enterprise.jst.server.sunappsrv;


import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

public class GlassFishClasspathContainerInitializer  extends ClasspathContainerInitializer {

	public void initialize(IPath containerPath, IJavaProject project) throws CoreException {
		IClasspathContainer container = new GlassFishClasspathContainer(containerPath );
		JavaCore.setClasspathContainer(containerPath, 
				new IJavaProject[] {project}, 
				new IClasspathContainer[] {container},
				null);
		
		
	
		
		
	}
	
	
}
