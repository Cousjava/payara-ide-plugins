/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 * 
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 * 
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.sun.sip;

import java.io.File;
import java.io.IOException;
import javax.xml.parsers.ParserConfigurationException;
import org.netbeans.modules.sun.sip.catalog.SIPCatalog;
import org.netbeans.modules.sun.sip.dd.SipApp;
import org.openide.ErrorManager;
import org.xml.sax.SAXException;

/**
 *
 * @author ludo
 */
public class ConfigProvider  {
    SipApp sipConfig;
    /** Creates a new instance of DDProvider */
    public ConfigProvider() {
        sipConfig =  SipApp.createGraph();
        sipConfig.graphManager().setDoctype(SIPCatalog.SIP_ID_1_0,
                "http://www.jcp.org/dtd/sip-app_1_0.dtd");
    }
    
    public SipApp getSipApp(){
        return sipConfig;
        
    }
    public void readFromFile(File inFile) {
       
        try {
            sipConfig = SipApp.createGraph(inFile);
        } catch(IOException ioe) {
            ErrorManager.getDefault().notify(ioe);
        } 
    } 
        public void save(File outFile) {

        
        try {
            sipConfig.write(outFile);
        } catch(IOException ioe) {
            ErrorManager.getDefault().notify(ioe);
        } 
    } 
}
