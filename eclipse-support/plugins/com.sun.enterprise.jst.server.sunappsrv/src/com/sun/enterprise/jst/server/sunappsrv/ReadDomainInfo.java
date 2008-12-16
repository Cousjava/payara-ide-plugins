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

package com.sun.enterprise.jst.server.sunappsrv;


import java.io.File;
//import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
/*import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
*/
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 *
 * @author Ludovic Champenois
 */
public class ReadDomainInfo {
    
    String adminPort="1";
    String serverPort="2";
    
    public  ReadDomainInfo(String loc, String domainName) {
        String domainScriptFilePath = loc+"/domains/" + domainName +
                "/config/domain.xml";                                           //NOI18N
        
   ///     File domainScriptFile = new File(domainScriptFilePath);
        
        // Load domain.xml
        Document domainScriptDocument = loadDomainScriptFile(domainScriptFilePath);
        if (domainScriptDocument == null) return ;
        
        
        
        // Find the "http-listener" element
        NodeList httplistenerNodeList = domainScriptDocument.getElementsByTagName("http-listener");
        if (httplistenerNodeList == null || httplistenerNodeList.getLength() == 0) {
            System.err.println("ConfigFilesUtils: cannot find 'http-listener' section in domain config file " + domainScriptFilePath);
            return ;
        }
        for (int i=0;i<httplistenerNodeList.getLength();i++){
            Element n = (Element) httplistenerNodeList.item(i);
            String p =n.getAttribute("id");
            if ("http-listener-1".equals(p)){
                serverPort = p;
            }
            if ("admin-listener".equals(p)){
                adminPort = p;
            }
            
            
        }
    }
    
    public String getAdminPort(){
        return adminPort;
        
    }
    public String getServerPort(){
        return serverPort;
        
    }
    
    
// creates Document instance from domain.xml
    private static Document loadDomainScriptFile(String domainScriptFilePath) {
        
       // Document document = null;
        
        try {
            
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            dbFactory.setValidating(false);
            
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            
            dBuilder.setEntityResolver(new EntityResolver() {
                public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {
                    StringReader reader = new StringReader("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"); // NOI18N
                    InputSource source = new InputSource(reader);
                    source.setPublicId(publicId);
                    source.setSystemId(systemId);
                    return source;
                }
            });
            
            return dBuilder.parse(new File(domainScriptFilePath));
            
        } catch (Exception e) {
            System.err.println("ConfigFilesUtils: unable to parse domain config file " + domainScriptFilePath);
            return null;
        }
        
    }
    
// saves Document to domain.xml
 /*   private static boolean saveDomainScriptFile(Document domainScriptDocument, String domainScriptFilePath) {
        boolean result = false;
        
        FileWriter domainScriptFileWriter = null;
        
        try {
            
            domainScriptFileWriter = new FileWriter(domainScriptFilePath);
            
            try {
                TransformerFactory transformerFactory = TransformerFactory.newInstance();
                Transformer transformer = transformerFactory.newTransformer();
                transformer.setOutputProperty(OutputKeys.INDENT, "yes");
                transformer.setOutputProperty(OutputKeys.METHOD, "xml");
                transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC, domainScriptDocument.getDoctype().getPublicId());
                transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, domainScriptDocument.getDoctype().getSystemId());
                transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
                
                DOMSource domSource = new DOMSource(domainScriptDocument);
                StreamResult streamResult = new StreamResult(domainScriptFileWriter);
                
                transformer.transform(domSource, streamResult);
                result = true;
            } catch (Exception e) {
                System.err.println("ConfigFilesUtils: Unable to save domain config file " + domainScriptFilePath);
                result = false;
            }
            
        } catch (IOException ioex) {
            System.err.println("ConfigFilesUtils: cannot create output stream for domain config file " + domainScriptFilePath);
            result = false;
        } finally {
            try { if (domainScriptFileWriter != null) domainScriptFileWriter.close(); } catch (IOException ioex2) { System.err.println("SunAS8IntegrationProvider: cannot close output stream for " + domainScriptFilePath); };
        }
        
        return result;
    }
    
    public static boolean isUnix() {
        return File.separatorChar == '/';
    }
    */
    
    
}
