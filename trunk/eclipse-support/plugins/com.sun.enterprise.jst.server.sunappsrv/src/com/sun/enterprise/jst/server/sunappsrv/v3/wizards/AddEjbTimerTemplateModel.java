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

import static org.eclipse.jst.j2ee.ejb.internal.operations.INewEnterpriseBeanClassDataModelProperties.EJB_NAME;
import static org.eclipse.jst.j2ee.ejb.internal.operations.INewEnterpriseBeanClassDataModelProperties.MAPPED_NAME;

import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.jst.j2ee.ejb.internal.operations.CreateEnterpriseBeanTemplateModel;
import org.eclipse.jst.j2ee.internal.common.operations.Method;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;

@SuppressWarnings("restriction")
public class AddEjbTimerTemplateModel extends CreateEnterpriseBeanTemplateModel {

	public static final String QUALIFIED_SCHEDULE = "javax.ejb.Schedule"; //$NON-NLS-1$
	public static final String QUALIFIED_TIMER = "javax.ejb.Timer"; //$NON-NLS-1$
	public static final String QUALIFIED_STATELESS = "javax.ejb.Stateless"; //$NON-NLS-1$

	public static final String SCHEDULED_TIMEOUT = "scheduledTimeout"; //$NON-NLS-1$

	public AddEjbTimerTemplateModel(IDataModel dataModel) {
		super(dataModel);
	}

	@Override
	public Collection<String> getImports() {
		Collection<String> collection = super.getImports();

		collection.add(QUALIFIED_SCHEDULE);
		collection.add(QUALIFIED_STATELESS);
		collection.add(QUALIFIED_TIMER);

		return collection;
	}

	public Map<String, String> getClassAnnotationParams() {
		Map<String, String> result = new Hashtable<String, String>();

		String dispName = getProperty(EJB_NAME).trim();
		if (!dispName.equals(getClassName()) && (dispName.length() > 0)) {
			result.put(ATT_NAME, QUOTATION_STRING + dispName + QUOTATION_STRING);
		}
		String mappedName = getProperty(MAPPED_NAME).trim();
		if (mappedName != null && mappedName.length() > 0) {
			result.put(ATT_MAPPED_NAME, QUOTATION_STRING + mappedName + QUOTATION_STRING);
		}

		return result;
	}

	public String getProperty(String propertyName) {
		return dataModel.getStringProperty(propertyName);
	}

	@Override
	public Collection<Method> getUnimplementedMethods() {
		Collection<Method> unimplementedMethods = super
				.getUnimplementedMethods();
		Iterator<Method> iterator = unimplementedMethods.iterator();

		while (iterator.hasNext()) {
			Method method = iterator.next();
			if (SCHEDULED_TIMEOUT.equals(method.getName())) {
				iterator.remove();
			}
		}

		return unimplementedMethods;
	}

}
