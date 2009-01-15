/*DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.

The contents of this file are subject to the terms of either the GNU
General Public License Version 2 only ("GPL") or the Common Development
and Distribution License("CDDL") (collectively, the "License").  You
may not use this file except in compliance with the License. You can obtain
a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
language governing permissions and limitations under the License.

When distributing the software, include this License Header Notice in each
file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
Sun designates this particular file as subject to the "Classpath" exception
as provided by Sun in the GPL Version 2 section of the License file that
accompanied this code.  If applicable, add the following below the License
Header, with the fields enclosed by brackets [] replaced by your own
identifying information: "Portions Copyrighted [year]
[name of copyright owner]"

Contributor(s):

If you wish your version of this file to be governed by only the CDDL or
only the GPL Version 2, indicate your decision by adding "[Contributor]
elects to include this software in this distribution under the [CDDL or GPL
Version 2] license."  If you don't indicate a single choice of license, a
recipient has the option to distribute your version of this file under
either the CDDL, the GPL Version 2 or to extend the choice of license to
its licensees as provided above.  However, if you add GPL Version 2 code
and therefore, elected the GPL Version 2 license, then the option applies
only if the new code is made subject to such option by the copyright
holder.
 */
package com.sun.enterprise.jst.server.sunappsrv.register.splashHandlers;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class RegisterAccountPage extends WizardPage {

	private static final int FIELD_WIDTH = 150;
	private static final int LABEL_WITDH = 80;
	private Text tName;
	private Text tSurname;
	private Text tEmail;
	private Text tPassword;
	private Text tConfirm;
	private Text tCompanyName;
	private Text tCountry;

	protected RegisterAccountPage(String pageName) {
		super(pageName);
		setTitle("Personal Information");
		setDescription("Please enter your personal information");
	}

	public void createControl(Composite comp) {
		Composite composite = new Composite(comp, SWT.NONE);

		GridLayout layout = new GridLayout(2, false);
		layout.marginWidth = 2;
		layout.marginHeight = 2;
		composite.setLayout(layout);
		setControl(composite);

		createTextComposite(composite, "Email address", tEmail);
		createTextComposite(composite, "Password", tPassword, SWT.PASSWORD);
		createTextComposite(composite, "Retype password", tConfirm, SWT.PASSWORD);
		createTextComposite(composite, "Name", tName);
		createTextComposite(composite, "Last Name", tSurname);
		createTextComposite(composite, "Company Name", tCompanyName);
		createTextComposite(composite, "Country", tCountry);
	}

	public void createTextComposite(Composite c, String labelText, Text text) {
		createTextComposite(c, labelText, text, 0);
	}

	public void createTextComposite(Composite c, String labelText, Text text, int i) {
		Label l1 = new Label(c, SWT.NONE);
		Text t = new Text(c, SWT.BORDER | i);
		GridData gridData = new GridData();
		gridData.widthHint = FIELD_WIDTH;
		t.setLayoutData(gridData);
		l1.setText(labelText);
		text = t;
	}
}
