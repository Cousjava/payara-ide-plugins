package com.sun.enterprise.jst.server.sunappsrv;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.ServerCore;
import org.eclipse.wst.server.core.internal.Messages;
import org.eclipse.wst.server.core.internal.ServerPlugin;

@SuppressWarnings("restriction")
public class GlassfishRuntime extends GenericServerRuntime {

	@Override
	public IStatus validate() {
		IStatus status = basicValidation();
		if (!status.isOK()) {
			return status;
		}
		
		return super.validate();
	}
	
	/**
	 * This is a copy paste of validate method from RuntimeDelegate.
	 * I have put it here because it is not called from validate method
	 * of GenericServerRuntime (I think that it should be...).
	 * 
	 * @return status of the runtime
	 */
	private IStatus basicValidation() {
		if (getRuntime().getName() == null || getRuntime().getName().length() == 0)
			return new Status(IStatus.ERROR, ServerPlugin.PLUGIN_ID, 0, Messages.errorRuntimeName, null);
		
		if (isNameInUse())
			return new Status(IStatus.ERROR, ServerPlugin.PLUGIN_ID, 0, Messages.errorDuplicateRuntimeName, null);
		
		IPath path = getRuntime().getLocation();
		if (path == null || path.isEmpty())
			return new Status(IStatus.ERROR, ServerPlugin.PLUGIN_ID, 0, "", null);
		
		return Status.OK_STATUS;
	}
	
	/**
	 * Returns <code>true</code> if the current name is already in use.
	 * 
	 * @return <code>true</code> if the name is in use, and <code>false</code>
	 *    otherwise
	 */
	private boolean isNameInUse() {
		IRuntime orig = getRuntime();
		if (getRuntimeWorkingCopy() != null)
			orig = getRuntimeWorkingCopy().getOriginal();
		
		IRuntime[] runtimes = ServerCore.getRuntimes();
		if (runtimes != null) {
			int size = runtimes.length;
			for (int i = 0; i < size; i++) {
				if (orig != runtimes[i] && getRuntime().getName().equals(runtimes[i].getName()))
					return true;
			}
		}
		return false;
	}
	
}
