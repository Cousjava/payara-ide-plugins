/*
 * Copyright (c) 2010 Oracle and/or its affiliates. All rights reserved.
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
	import org.eclipse.core.runtime.IProgressMonitor;
	import org.eclipse.jst.common.project.facet.core.libprov.LibraryProviderOperation;
	import org.eclipse.jst.common.project.facet.core.libprov.LibraryProviderOperationConfig;

	/**
	 * @author ludo>
	 */

	public class InstallJAXRS   extends LibraryProviderOperation{
	    public void execute( final LibraryProviderOperationConfig config,
	                         final IProgressMonitor monitor )

	        throws CoreException{
	        monitor.beginTask( "", 1 );

	            monitor.done();
	        }
	    }
	    
	
