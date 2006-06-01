// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * glassfishplugins/www/license/CDDLv1.0.txt or 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable, 
 * add the following below this CDDL HEADER, with the 
 * fields enclosed by brackets "[]" replaced with your 
 * own identifying information: Portions Copyright [yyyy] 
 * [name of copyright owner]
 */
// </editor-fold>



package com.sun.enterprise.jst.server.sunappsrv.ejbjar;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.netbeans.modules.j2ee.sun.dd.api.DDProvider;
import org.netbeans.modules.j2ee.sun.dd.api.ejb.EnterpriseBeans;
import org.netbeans.modules.j2ee.sun.dd.api.ejb.SunEjbJar;


/**
 * Helper class to access a sun-ejb-jar.xml file.
 */
public class SunEjbJarDeploymentPlan {
    
    
    private SunEjbJar swa;
    
    /**
     * Loads a SunEjbJar xml file from the given URL, and creates one if none exists now
     *
     * @param is
     * @throws Exception if anything goes wrong
     */
//    public SunEjbJarDeploymentPlan(InputStream is) throws Exception {
//        swa= DDProvider.getDefault().(is);
//        
//    }
    public SunEjbJar getSunEjbJar(){
        return swa;
        
    }
    /**
     * create a SunEjbJar xml file
     *
     * @throws Exception if anything goes wrong
     */
    public SunEjbJarDeploymentPlan() throws Exception {
        swa = (SunEjbJar) DDProvider.getDefault().newGraph(SunEjbJar.class);
        EnterpriseBeans eb = swa.newEnterpriseBeans();
        swa.setEnterpriseBeans(eb);
        
    }
    
    /**
     * Returns an input stream for writing to the disk with a local locale.
     *
     * @return java.io.InputStream
     * @throws IOException if anything goes wrong
     */
    public InputStream getInputStream() throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        swa.write(out);
        return new ByteArrayInputStream(out.toByteArray());
    }
    
    
}
