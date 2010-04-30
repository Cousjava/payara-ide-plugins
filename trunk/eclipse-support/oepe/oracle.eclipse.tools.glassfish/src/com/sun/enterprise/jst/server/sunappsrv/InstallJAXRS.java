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
	    
	