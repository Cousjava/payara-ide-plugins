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


package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.project.JavaEEProjectUtilities;
import org.eclipse.wst.common.componentcore.internal.util.IModuleConstants;
import org.eclipse.wst.server.core.IRuntime;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class WizardUtil {


	public static boolean isWebOrEJBProjectWithGF3Runtime(IProject project) {
		try {
			boolean result = project.isAccessible() && 
				project.hasNature(IModuleConstants.MODULE_NATURE_ID) && 
				(JavaEEProjectUtilities.isDynamicWebProject(project) ||
				JavaEEProjectUtilities.isEJBProject(project));

			if (result) {
				return hasGF3Runtime(project);
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}

		return false;
	}

	public static boolean isWebProjectWithGF3Runtime(IProject project) {
		try {
			boolean result = (project.isAccessible() && 
				project.hasNature(IModuleConstants.MODULE_NATURE_ID) && 
				JavaEEProjectUtilities.isDynamicWebProject(project));

			if (result) {
				return hasGF3Runtime(project);
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}

		return false;
	}

	public static boolean hasGF3Runtime(IProject project) {
		try {
			IRuntime runtime = J2EEProjectUtilities.getServerRuntime(project);
			if ((runtime != null) && SunAppSrvPlugin.is3OrAbove(runtime)){
				return true;
			}
               } catch (CoreException e) {
			e.printStackTrace();
		}

		return false;
	}
}
