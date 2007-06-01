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

package com.sun.enterprise.jst.server.sunappsrv;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jst.server.generic.core.internal.GenericServer;
import org.eclipse.wst.server.core.IServerWorkingCopy;




/* wrapper to get rw access to all the props defined in the serverdef files
 *
 **/

public class SunAppServer extends GenericServer {
    
    public static final String ROOTDIR = "sunappserver.rootdirectory";
    public static final String ADDRESS = "sunappserver.serveraddress";
    public static final String SERVERPORT = "sunappserver.serverportnumber";
    public static final String ADMINSERVERPORT = "sunappserver.adminserverportnumber";
    public static final String DOMAINNAME = "sunappserver.domainname";
    public static final String ADMINNAME = "sunappserver.adminname";
    public static final String ADMINPASSWORD = "sunappserver.adminpassword";
    
    public SunAppServer(){
    }
    
    
    
    
    public String getRootDir() {
        return (String) getServerInstanceProperties().get(ROOTDIR);
    }   
    
    public String getServerPort() {
        return (String) getServerInstanceProperties().get(SERVERPORT);
    }
    public void setServerPort(String value) {
        getServerInstanceProperties().put(SERVERPORT, value);
    }
    public String getAdminServerPort() {
        return (String) getServerInstanceProperties().get(ADMINSERVERPORT);
    }
    public void setAdminServerPort(String value) {
        getServerInstanceProperties().put(ADMINSERVERPORT, value);
    }
    
    public String getAdminName() {
        return (String) getServerInstanceProperties().get(ADMINNAME);
    }
    public void setAdminName(String value) {
        getServerInstanceProperties().put(ADMINNAME, value);
    }
    
    
    public String getdomainName() {
        return (String) getServerInstanceProperties().get(DOMAINNAME);
    }
    public String getServerAddress() {
        return (String) getServerInstanceProperties().get(ADDRESS);
    }
    
    public void  saveConfiguration(IProgressMonitor m) throws CoreException  {
        SunAppSrvPlugin.logMessage("in Save SunAppServer ");
        super.saveConfiguration(m);
    }
    
    
    public String getAdminPassword() {
        
        return (String) getServerInstanceProperties().get(ADMINPASSWORD);
    }
    public void setAdminPassword(String value) {
        getServerInstanceProperties().put(ADMINPASSWORD, value); 
        SunAppSrvPlugin.logMessage("In  setAdminPassword)"+ value);
        try {
            //this.saveConfiguration(new NullProgressMonitor());
            this.configurationChanged();
        } catch (Exception ex) {
            SunAppSrvPlugin.logMessage("error ="+ ex);
        }
    }
    
    
    public static SunAppServer getSunAppServer(IServerWorkingCopy server){
        SunAppServer  sunserver = (SunAppServer) server.getAdapter(SunAppServer.class);
        if (sunserver == null) {
            sunserver = (SunAppServer) server.loadAdapter(SunAppServer.class, new NullProgressMonitor());
        }
        return sunserver;
    }
    
    /**
     * 
     * @param host 
     * @param port 
     * @return 
     */
    public  boolean isRunning() {

        
        try {
            InetSocketAddress isa = new InetSocketAddress(getServerAddress(), Integer.parseInt(getServerPort()));
            Socket socket = new Socket();
            socket.connect(isa, 1);
            socket.close();
            return true;
        } catch (Exception e) {
            return false;
        }
    }
    
    
    
    
}