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

package com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards;



public class Messages extends org.eclipse.osgi.util.NLS {
    static {
        org.eclipse.osgi.util.NLS.initializeMessages(
                "com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards.Messages", Messages.class);
    }
    
    public static String Connection;
	public static String Create;
	public static String JNDIName;
	public static String ProjectName;
    public static String wizardTitle;
    public static String wizardDescription;
    public static String ErrorTitle;
    public static String errorUnknown;
    public static String errorFileExists;
    public static String errorFolderMissing;
    public static String errorProjectMissing;
    public static String errorConnectionMissing;
    public static String errorConnectionInvalid;
    
    //JavaMail Wizard
    public static String mailWizardTitle;
    public static String mailWizardDescription;
	public static String MailHost;
	public static String MailUser;
	public static String MailFrom;
	public static String errorMailHostNameMissing;
	public static String errorMailUserNameMissing;
	public static String errorMailReturnAddrMissing;
	
	//JMS Wizard
	public static String jmsWizardTitle;
	public static String jmsWizardDescription;
	public static String lblChooseType;
	public static String lblAdminObject;
	public static String lblConnector;
	public static String lblQueue;
	public static String lblTopic;
	public static String lblQueueConnectionFactory;
	public static String lblTopicConnectionFactory;
	public static String lblConnectionFactory;
	
	//Common
	public static String errorJndiNameMissing;
	public static String errorResourceTypeMissing;
	public static String errorFolderNull;
	public static String errorDuplicateName;
}
