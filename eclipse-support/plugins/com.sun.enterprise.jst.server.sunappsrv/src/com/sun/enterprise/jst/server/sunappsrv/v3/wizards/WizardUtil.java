package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.project.JavaEEProjectUtilities;
import org.eclipse.wst.common.componentcore.internal.util.IModuleConstants;
import org.eclipse.wst.server.core.IRuntime;

public class WizardUtil {

	private static final String V3_RUNTIME = "com.sun.enterprise.jst.server.runtime.sunappsrv92"; //$NON-NLS-1$

	public static boolean hasGF3Runtime(IProject project) {
		// super's test for isProjectValid requires an ejb project and we don't 
		// want to do that, so result is basically a test of the grandsuper's isProjectValid
		// with the addition of allowing both ejb and web projects
		try {
			boolean result = project.isAccessible() && 
				project.hasNature(IModuleConstants.MODULE_NATURE_ID) && 
				(JavaEEProjectUtilities.isDynamicWebProject(project) ||
				JavaEEProjectUtilities.isEJBProject(project));

			if (result) {
				IRuntime runtime = J2EEProjectUtilities.getServerRuntime(project);
				if ((runtime != null) && runtime.getRuntimeType().getId().equals(V3_RUNTIME)){
					return true;
				}
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}

		return false;
	}

}
