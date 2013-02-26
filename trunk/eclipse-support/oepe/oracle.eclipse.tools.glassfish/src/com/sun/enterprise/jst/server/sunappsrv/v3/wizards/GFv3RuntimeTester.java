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

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class GFv3RuntimeTester extends PropertyTester {

	//@Override
	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		try {
			final IProject pj = ((IResource) receiver).getProject();

			if (pj == null) {
				return false;
			}

			boolean expected = expectedValue instanceof Boolean ? ((Boolean) expectedValue)
					.booleanValue()
					: Boolean.parseBoolean((String) expectedValue);

			if (property.equals("enableForGFv3Web")) { //$NON-NLS-1$
				return WizardUtil.isWebProjectWithGF3Runtime(pj) == expected;
			} else if (property.equals("enableForGFv3WebOrEJB")) { //$NON-NLS-1$
				return WizardUtil.isWebOrEJBProjectWithGF3Runtime(pj) == expected;
			}
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage(
					"failed to check GFv3 enablement property", e);
		}
		return false;
	}

}
