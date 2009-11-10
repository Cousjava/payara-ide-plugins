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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;

import org.eclipse.jst.j2ee.internal.web.operations.CreateWebClassTemplateModel;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;

@SuppressWarnings("restriction")
public class AddGenericResourceTemplateModel extends CreateWebClassTemplateModel {

	public static final String QUALIFIED_CONTEXT = "javax.ws.rs.core.Context"; //$NON-NLS-1$
	public static final String QUALIFIED_URI_INFO = "javax.ws.rs.core.UriInfo"; //$NON-NLS-1$
	public static final String QUALIFIED_CONSUMES = "javax.ws.rs.Consumes"; //$NON-NLS-1$
	public static final String QUALIFIED_PUT = "javax.ws.rs.PUT"; //$NON-NLS-1$
	public static final String QUALIFIED_PATH = "javax.ws.rs.Path"; //$NON-NLS-1$
	public static final String QUALIFIED_GET = "javax.ws.rs.GET"; //$NON-NLS-1$
	public static final String QUALIFIED_PRODUCES = "javax.ws.rs.Produces"; //$NON-NLS-1$

	protected static final String TYPE_APP_JSON = "application/json"; //$NON-NLS-1$
	protected static final String TYPE_APP_XML = "application/xml"; //$NON-NLS-1$
	protected static final String TYPE_TEXT_HTML = "text/html"; //$NON-NLS-1$
	protected static final String TYPE_TEXT_PLAIN = "text/plain"; //$NON-NLS-1$

	protected static final String SUFFIX_JSON = "Json"; //$NON-NLS-1$
	protected static final String SUFFIX_XML = "Xml"; //$NON-NLS-1$
	protected static final String SUFFIX_HTML = "Html"; //$NON-NLS-1$
	protected static final String SUFFIX_TEXT = "Text"; //$NON-NLS-1$


	private static final Map<String, String> typeToSuffix;
	
	static {
		typeToSuffix= new HashMap<String, String>();
		typeToSuffix.put(TYPE_APP_JSON, SUFFIX_JSON);
		typeToSuffix.put(TYPE_APP_XML, SUFFIX_XML);
		typeToSuffix.put(TYPE_TEXT_HTML, SUFFIX_HTML);
		typeToSuffix.put(TYPE_TEXT_PLAIN, SUFFIX_TEXT);
	}

	public AddGenericResourceTemplateModel(IDataModel dataModel) {
		super(dataModel);
	}

	@Override
	public Collection<String> getImports() {
		Collection<String> collection = super.getImports();

		collection.add(QUALIFIED_CONTEXT);
		collection.add(QUALIFIED_URI_INFO);
		collection.add(QUALIFIED_CONSUMES);
		collection.add(QUALIFIED_PUT);
		collection.add(QUALIFIED_PATH);
		collection.add(QUALIFIED_GET);
		collection.add(QUALIFIED_PRODUCES);

		// if repClass is not in java.lang, add an import for it as well
		// actually, the return from super.getImports returns a collection which is 
		// smart enough to skip adding java.lang classes
		String repClass = getProperty(AddGenericResourceDataModelProvider.REPRESENTATION_CLASS);

		if (repClass != null) {
			// so, this is all we should need to do
			collection.add(repClass);
			// however, ImportsCollection has a bug that also skips adding
			// classes like java.lang.reflect.Method, so we need to do this as 
			// a workaround (for eclipse bug 294688)
			int index = repClass.lastIndexOf("."); //$NON-NLS-1$
			if (index != -1) {
				String packageName = repClass.substring(0, index);
				if (repClass.startsWith("java.lang.") && //$NON-NLS-1$
						!packageName.equals("java.lang")) { //$NON-NLS-1$
					Collection<String> myCollection = new TreeSet<String>();
					myCollection.addAll(collection);
					myCollection.add(repClass);
					return myCollection;
				}
			} // end workaround
		}

		return collection;
	}

	public String getProperty(String propertyName) {
		return dataModel.getStringProperty(propertyName);
	}

	public String getMethodNameSuffixFromMimeType() {
		String mimeType = getProperty(AddGenericResourceDataModelProvider.MIME_TYPE);

		return ((mimeType != null) ? typeToSuffix.get(mimeType) : null);
	}

	public String getUnqualifiedRepresentationClass() {
		String repClass = getProperty(AddGenericResourceDataModelProvider.REPRESENTATION_CLASS);

		if (repClass != null) {
			int index = repClass.lastIndexOf("."); //$NON-NLS-1$
			if (index != -1) {
				return repClass.substring(index + 1);
			}
		}
		return null;
	}
}
