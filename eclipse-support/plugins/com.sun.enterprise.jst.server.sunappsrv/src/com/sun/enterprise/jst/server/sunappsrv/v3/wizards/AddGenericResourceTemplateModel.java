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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeSet;

import org.eclipse.jst.j2ee.internal.common.operations.INewJavaClassDataModelProperties;
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
	public static final String QUALIFIED_DELETE = "javax.ws.rs.DELETE"; //$NON-NLS-1$
	public static final String QUALIFIED_PRODUCES = "javax.ws.rs.Produces"; //$NON-NLS-1$
	public static final String QUALIFIED_PATH_PARAM = "javax.ws.rs.PathParam"; //$NON-NLS-1$
	public static final String QUALIFIED_POST = "javax.ws.rs.POST"; //$NON-NLS-1$
	public static final String QUALIFIED_RESPONSE = "javax.ws.rs.core.Response"; //$NON-NLS-1$

	protected static final String TYPE_APP_JSON = "application/json"; //$NON-NLS-1$
	protected static final String TYPE_APP_XML = "application/xml"; //$NON-NLS-1$
	protected static final String TYPE_TEXT_HTML = "text/html"; //$NON-NLS-1$
	protected static final String TYPE_TEXT_PLAIN = "text/plain"; //$NON-NLS-1$

	protected static final String SUFFIX_JSON = "Json"; //$NON-NLS-1$
	protected static final String SUFFIX_XML = "Xml"; //$NON-NLS-1$
	protected static final String SUFFIX_HTML = "Html"; //$NON-NLS-1$
	protected static final String SUFFIX_TEXT = "Text"; //$NON-NLS-1$

	protected static final String SIMPLE_PATTERN = "Simple"; //$NON-NLS-1$
	protected static final String CONTAINER_PATTERN = "Container"; //$NON-NLS-1$

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
		boolean isContainerClass = isContainerClass();
		boolean usesContext = isSimplePattern() || isContainerClass;

		collection.add(QUALIFIED_CONSUMES);
		collection.add(QUALIFIED_GET);
		collection.add(QUALIFIED_PRODUCES);
		
		if (usesContext) {
			collection.add(QUALIFIED_CONTEXT);
			collection.add(QUALIFIED_URI_INFO);	
			collection.add(QUALIFIED_PATH);
		} else {
			collection.add(QUALIFIED_DELETE);
		}
		if (isContainerClass) {
			collection.add(QUALIFIED_POST);
			collection.add(QUALIFIED_RESPONSE);
			if (getParamList() != null) {
				collection.add(QUALIFIED_PATH_PARAM);
			}
		} else {
			collection.add(QUALIFIED_PUT);
		}

		// if repClass is not in java.lang, add an import for it as well
		// actually, the return from super.getImports returns a collection which is 
		// smart enough to skip adding java.lang classes
		String repPropName = (isContainerClass ? AddGenericResourceDataModelProvider.CONTAINER_REPRESENTATION_CLASS : 
			AddGenericResourceDataModelProvider.REPRESENTATION_CLASS);
		String repClass = getProperty(repPropName);

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

	protected boolean isSimplePattern() {
		String patternProp = getProperty(AddGenericResourceDataModelProvider.PATTERN);
		return patternProp.endsWith(SIMPLE_PATTERN);
	}

	public String getProperty(String propertyName) {
		return dataModel.getStringProperty(propertyName);
	}

	protected String getMethodNameSuffixFromMimeType() {
		String mimeType = getProperty(AddGenericResourceDataModelProvider.MIME_TYPE);

		return ((mimeType != null) ? typeToSuffix.get(mimeType) : null);
	}

	protected String getUnqualifiedRepresentationClass() {
		return getUnqualifiedRepresentationClass(AddGenericResourceDataModelProvider.REPRESENTATION_CLASS);
	}

	protected String getUnqualifiedContainerRepresentationClass() {
		return getUnqualifiedRepresentationClass(AddGenericResourceDataModelProvider.CONTAINER_REPRESENTATION_CLASS);
	}

	protected String getUnqualifiedRepresentationClass(String propertyName) {
		String repClass = getProperty(propertyName);

		if (repClass != null) {
			int index = repClass.lastIndexOf("."); //$NON-NLS-1$
			if (index != -1) {
				return repClass.substring(index + 1);
			}
		}
		return null;
	}

	// assumes that it has passed validation
	protected String[] getParamList() {
		if (!isSimplePattern()) {
			String path = getProperty(AddGenericResourceDataModelProvider.PATH);
			StringTokenizer segments = new StringTokenizer(path, "/ "); //$NON-NLS-1$
			List<String> pathParts = new ArrayList<String>();
			while (segments.hasMoreTokens()) {
				String segment = segments.nextToken();
				if (segment.startsWith("{")) { //$NON-NLS-1$
					if (segment.length() > 2 && segment.endsWith("}")) { //$NON-NLS-1$
						pathParts.add(segment.substring(1, segment.length() - 1));
					}
				}
			}
			return (pathParts.isEmpty() ? null : (String[])pathParts.toArray(new String[]{}));
		}
		return null;
	}

	protected String getCommaSeparatedParamList() {
		return buildCommaSeparatedParamList(null);
	}

	protected String getCommaSeparatedParamListWithTypes() {
		return buildCommaSeparatedParamList("String"); //$NON-NLS-1$
	}

	private String buildCommaSeparatedParamList(String additionalString) {
		String[] paramList = getParamList();
		StringBuffer buffer = new StringBuffer();

		if (paramList != null) {
			int count = paramList.length;

			for (int i = 0; i < count; i++) {
				String string = paramList[i];
				if (additionalString != null) {
					buffer.append(additionalString);
					buffer.append(" "); //$NON-NLS-1$
				}
				buffer.append(string);
				if (i < count - 1) {
					buffer.append(", "); //$NON-NLS-1$
				}
			}
			return buffer.toString();
		}
		return null;
	}

	protected void setIsContainerClass() {
		String existingClassName = getProperty(INewJavaClassDataModelProperties.CLASS_NAME);
		dataModel.setBooleanProperty(AddGenericResourceDataModelProvider.IN_CONTAINER_CLASS, Boolean.TRUE);
		dataModel.setProperty(INewJavaClassDataModelProperties.CLASS_NAME, existingClassName + CONTAINER_PATTERN);
		dataModel.setProperty(AddGenericResourceDataModelProvider.ORIGINAL_CLASS_NAME, existingClassName);
	}

	private boolean isContainerClass() {
		return dataModel.getBooleanProperty(AddGenericResourceDataModelProvider.IN_CONTAINER_CLASS);
	}
}
