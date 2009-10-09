package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

public class GFv3RuntimeTester extends PropertyTester {

	@Override
	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		final IProject pj = ((IResource) receiver).getProject();

		if (pj == null) {
			return false;
		}

		boolean expected =  expectedValue instanceof Boolean ? ((Boolean) expectedValue).booleanValue() :
			Boolean.parseBoolean((String) expectedValue);
		
		return WizardUtil.isWebOrEJBProjectWithGF3Runtime(pj) == expected;
	}

}
