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

package com.sun.enterprise.jst.server.sunappsrv.actions;

import java.io.File;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 * small action to test if the module is functional. Need te be removed later, when we are done
 * debugging
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class PreludeUpdateCenterAction extends OpenBrowserEditorAction {

    /**
     * The constructor.
     */
    public PreludeUpdateCenterAction() {
    	super ("GlassFish Update Center...",getImageDescriptorFromlocalImage("icons/obj16/updateCenter.png"));
    }


    public void execute(IServer server) {
    	SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter( SunAppServerBehaviour.class, null);
    	if (!sab.isV3()) { // V2 only
			String loc=sab.getSunApplicationServerInstallationDirectory()+"/updatecenter/bin/updatetool";    	
       	    if (File.separator.equals("\\")) {
       			loc = loc + ".bat"; //NOI18N
       	    }
    		String[] command = new String[]{
    				loc    		}; 
    		try {
    			Execute.launch(null, command, null, new File(sab.getSunApplicationServerInstallationDirectory()), true);
    			return;
    		} catch (Exception ioe) {
    			SunAppSrvPlugin.logMessage("error Launching Executable", ioe);
    		}
    	} else { // V3
			if (accept(server)==false){
				showMessageDialog();
				return;
			}
			showPageInBrowser(server);
    	}
    }

	protected String getEditorClassName() { return "com.sun.enterprise.jst.server.sunappsrv.v3.UpdateCenter"; }

 	protected String getIconName() { return "icons/obj16/sunappsrv.gif"; }

 	protected String getURL() { return new com.sun.enterprise.jst.server.sunappsrv.v3.UpdateCenter().getURL(); }

    @Override
    public boolean accept(IServer server) {
        return acceptIfServerRunning(server);
    }
}
