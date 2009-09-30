// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.project.JavaEEProjectUtilities;
import org.eclipse.wst.common.componentcore.internal.util.IModuleConstants;
import org.eclipse.wst.server.core.IRuntime;

@SuppressWarnings("restriction")
public class WizardUtil {

	private static final String V3_RUNTIME = "com.sun.enterprise.jst.server.runtime.sunappsrv92"; //$NON-NLS-1$

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

	public static boolean hasGF3Runtime(IProject project) {
		try {
			IRuntime runtime = J2EEProjectUtilities.getServerRuntime(project);
			if ((runtime != null) && runtime.getRuntimeType().getId().equals(V3_RUNTIME)){
				return true;
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}

		return false;
	}
}
