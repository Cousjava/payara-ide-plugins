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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wst.html.ui.internal.wizard.NewHTMLTemplatesWizardPage;

@SuppressWarnings("restriction")
public class NewXHTMLTemplatesWizardPage extends NewHTMLTemplatesWizardPage {

	/**
	 * Content provider for xhtml templates
	 */
	private class XTemplateContentProvider implements IStructuredContentProvider {
		private IContentProvider delegate;

		public XTemplateContentProvider(IContentProvider delegate) {
			this.delegate = delegate;
		}
		
		/*
		 * @see IContentProvider#dispose()
		 */
		public void dispose() {
			delegate.dispose();
			delegate = null;
		}

		/*
		 * @see IStructuredContentProvider#getElements(Object)
		 */
		public Object[] getElements(Object input) {
			Object[] templs = ((IStructuredContentProvider) delegate).getElements(input);
			List<Object> result = new ArrayList<Object>(templs.length);
			for (Object templ : templs) {
				if (templ instanceof Template) {
					String name = ((Template) templ).getName().toLowerCase();
					if (name.indexOf("xhtml") >= 0 || name.indexOf("facelet") >= 0) {
						result.add(templ);
					}
				}
			}
			return result.toArray(new Object[result.size()]);
		}

		/*
		 * @see IContentProvider#inputChanged(Viewer, Object, Object)
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			delegate.inputChanged(viewer, oldInput, newInput);
		}
	}

	public void createControl(Composite ancestor) {
		super.createControl(ancestor);
		
		try {
			Field declaredField = getClass().getSuperclass().getDeclaredField("fTableViewer");
			declaredField.setAccessible(true);
			TableViewer v = ((TableViewer) declaredField.get(this));
			v.setContentProvider(new XTemplateContentProvider(v.getContentProvider()));

//			declaredField = getClass().getSuperclass().getDeclaredField("fUseTemplateButton");
//			declaredField.setAccessible(true);
//			Button b = ((Button) declaredField.get(this));
//			b.setEnabled(true);
//			b.setVisible(false);
//			GridData gd = ((GridData) b.getLayoutData());
//			gd.exclude = true;
//			b.setLayoutData(gd);
			
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (NoSuchFieldException e) {
			e.printStackTrace();
		}
	}
}
