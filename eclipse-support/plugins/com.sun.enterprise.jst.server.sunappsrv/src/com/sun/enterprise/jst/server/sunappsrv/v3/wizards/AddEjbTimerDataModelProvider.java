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

import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jst.j2ee.ejb.internal.operations.AddEnterpriseBeanOperation;
import org.eclipse.jst.j2ee.ejb.internal.operations.NewEnterpriseBeanClassDataModelProvider;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaClassDataModelProvider;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaEEArtifactClassOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class AddEjbTimerDataModelProvider extends
		NewEnterpriseBeanClassDataModelProvider {

	public static final String SCHEDULE = "AddEjbTimer.SCHEDULE";

	public IDataModelOperation getDefaultOperation() {
		return new AddEnterpriseBeanOperation(getDataModel()) {

			@Override
			protected NewJavaEEArtifactClassOperation getNewClassOperation() {
				return new AddEjbTimerClassOperation(getDataModel());
			}
		};
	}

	/**
	 * Subclasses may extend this method to add their own data model's
	 * properties as valid base properties.
	 * 
	 * @see org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider#getPropertyNames()
	 */
	@Override
	public Set<String> getPropertyNames() {
		// Add Bean specific properties defined in this data model
		Set<String> propertyNames = super.getPropertyNames();

		propertyNames.add(SCHEDULE);

		return propertyNames;
	}

	/**
	 * Subclasses may extend this method to provide their own default values for
	 * any of the properties in the data model hierarchy. This method does not
	 * accept a null parameter. It may return null.
	 * 
	 * @see NewJavaClassDataModelProvider#getDefaultProperty(String)
	 * @see IDataModelProvider#getDefaultProperty(String)
	 * 
	 * @param propertyName
	 * @return Object default value of property
	 */
	@Override
	public Object getDefaultProperty(String propertyName) {
		if (propertyName.equals(SCHEDULE)) {
			return Messages.timerScheduleDefault;
		}
		
		// Otherwise check super for default value for property
		return super.getDefaultProperty(propertyName);
	}

	public IStatus validate(String propertyName) {
		if (SCHEDULE.equals(propertyName)) {
			String value = (String) getProperty(SCHEDULE);
			if (value == null || value.trim().length() == 0) {
				return SunAppSrvPlugin.createErrorStatus(
						"The schedule cannot be empty.", null); // TODO i18n
			}
		}
		IStatus status = super.validate(propertyName);
		return status;
	}

	/**
	 * Super method breaks with CCE when we have non-EJB project, so leave it for the future to fix itself and no check atm.
	 */
	@Override
	protected IStatus validateEjbName() {
		try {
			return super.validateEjbName();
		} catch (ClassCastException e) {
			// ignore atm
			// SunAppSrvPlugin.logMessage("failed to validate EjbName", e);
			return Status.OK_STATUS;
		}
	}

}
