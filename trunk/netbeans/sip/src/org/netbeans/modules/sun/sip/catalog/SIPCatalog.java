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


package org.netbeans.modules.sun.sip.catalog;

import java.awt.Image;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.netbeans.modules.xml.catalog.spi.CatalogDescriptor;
import org.netbeans.modules.xml.catalog.spi.CatalogListener;
import org.netbeans.modules.xml.catalog.spi.CatalogReader;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

/**
 * Catalog for SIP DTD.
 *
 * @author Ludo
 */
public class SIPCatalog implements CatalogReader, CatalogDescriptor, org.xml.sax.EntityResolver {
    
    public static final String SIP_ID_1_0 = "-//Java Community Process//DTD SIP Application 1.0//EN"; // NOI18N
            
    private static final String URL_SIP_1_0 ="jar:nbinst:///modules/ext/sip/ssa-api.jar!/sip-app_1_0.dtd"; // NOI18N
    
    /**
     * Creates a new instance of SIPCatalog
     */
    public SIPCatalog() {
    }
    
    /**
     * Get String iterator representing all public IDs registered in catalog.
     * @return null if cannot proceed, try later.
     */
    public java.util.Iterator getPublicIDs() {
        java.util.List list = new java.util.ArrayList();
        list.add(SIP_ID_1_0);


        return list.listIterator();
    }
    
    /**
     * Get registered systemid for given public Id or null if not registered.
     * @return null if not registered
     */
    public String getSystemID(String publicId) {
        if (SIP_ID_1_0.equals(publicId))
            return URL_SIP_1_0;


        else return null;
    }
    
    /**
     * Refresh content according to content of mounted catalog.
     */
    public void refresh() {
    }
    
    /**
     * Optional operation allowing to listen at catalog for changes.
     * @throws UnsupportedOpertaionException if not supported by the implementation.
     */
    public void addCatalogListener(CatalogListener l) {
    }
    
    /**
     * Optional operation couled with addCatalogListener.
     * @throws UnsupportedOpertaionException if not supported by the implementation.
     */
    public void removeCatalogListener(CatalogListener l) {
    }
    
    /** Registers new listener.  */
    public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
    }
    
     /** Unregister the listener.  */
    public void removePropertyChangeListener(java.beans.PropertyChangeListener l) {
    }
    
    /**
     * @return I18N display name
     */
    public String getDisplayName() {
        return NbBundle.getMessage (SIPCatalog.class, "LBL_SIPCatalog");  //NOI18N
    }
    
    /**
     * Return visuaized state of given catalog.
     * @param type of icon defined by JavaBeans specs
     * @return icon representing current state or null
     */
    public java.awt.Image getIcon(int type) {
        return Utilities.loadImage("org/netbeans/modules/sun/sip/catalog/SIPIcon.gif"); // NOI18N
    }
    
    /**
     * @return I18N short description
     */
    public String getShortDescription() {
        return NbBundle.getMessage (SIPCatalog.class, "DESC_SIPCatalog");     //NOI18N
    }
    
   /**
     * Resolves schema definition file for SIP
     * @param publicId publicId for resolved entity (null in our case)
     * @param systemId systemId for resolved entity
     * @return InputSource for publisId, 
     */    
    public org.xml.sax.InputSource resolveEntity(String publicId, String systemId) throws org.xml.sax.SAXException, java.io.IOException {
       if (SIP_ID_1_0.equals(publicId)) {
            return new org.xml.sax.InputSource(URL_SIP_1_0);
        }   else {
            return null;
        }
    }
    
    /**
     * Get registered URI for the given name or null if not registered.
     * @return null if not registered
     */
    public String resolveURI(String name) {
        return null;
    }
    /**
     * Get registered URI for the given publicId or null if not registered.
     * @return null if not registered
     */ 
    public String resolvePublic(String publicId) {
        return null;
    }
}
