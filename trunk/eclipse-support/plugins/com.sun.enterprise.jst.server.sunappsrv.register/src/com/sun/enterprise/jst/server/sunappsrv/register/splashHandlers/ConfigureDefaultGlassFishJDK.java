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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ConfigureDefaultGlassFishJDK {
    
    private static final String ASENV_INSERTION_POINT_WIN_STRING    = "set AS_JAVA";
    private static final String ASENV_INSERTION_POINT_NOWIN_STRING  = "AS_JAVA";
    
    
    // replaces the AS_JAVA item in asenv.bat/conf
    static public boolean modifyAsEnvScriptFile(String installRoot, String targetJavaHomePath) {
        
        String ext = (isUnix() ? "conf" : "bat");

        String asEnvScriptFilePath  = installRoot+"/glassfish/config/asenv." + ext;
        File asEnvScriptFile = new File(asEnvScriptFilePath);
        if (!asEnvScriptFile.canWrite()) {
            Logger.getLogger(ConfigureDefaultGlassFishJDK.class.getName()).log(Level.FINER,"asenv issue");
            return false;
        }
        String lineBreak = System.getProperty("line.separator");
        BufferedReader br = null;
        FileWriter fw = null;
        try {
            
            String line;
            FileReader fr = new FileReader(asEnvScriptFile);
            br = new BufferedReader(fr);
            StringBuilder buffer = new StringBuilder(Math.min(asEnvScriptFilePath.length(), 60000));
            
            String asJavaString = (isUnix() ? ASENV_INSERTION_POINT_NOWIN_STRING : ASENV_INSERTION_POINT_WIN_STRING);
            
            // copy config file from disk into memory buffer and modify line containing AS_JAVA definition
            while ((line = br.readLine()) != null) {
                if (line.trim().startsWith(asJavaString)) {
                    buffer.append(asJavaString);
                    buffer.append('=');
                    buffer.append(targetJavaHomePath);
                } else {
                    buffer.append(line);
                }
                buffer.append(lineBreak);
            }
            //br.close();
            
            // flush modified config file from memory buffer back to disk
            fw = new FileWriter(asEnvScriptFile);
            fw.write(buffer.toString());
            fw.flush();
            //fw.close();
            
            if (isUnix()) {
                Runtime.getRuntime().exec("chmod a+r " + asEnvScriptFile.getAbsolutePath()); //NOI18N
            }
            
            return true;
            
        } catch (RuntimeException re) {
            Logger.getLogger(ConfigureDefaultGlassFishJDK.class.getName()).log(Level.FINER,"",re);
            return false;
        } catch (Exception ex) {
            Logger.getLogger(ConfigureDefaultGlassFishJDK.class.getName()).log(Level.FINER,"",ex);
            return false;
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException ioe) {
                    Logger.getLogger(ConfigureDefaultGlassFishJDK.class.getName()).log(Level.FINEST,"",ioe);
                }
            }
            if (fw != null) {
                try {
                    fw.close();
                } catch (IOException ioe) {
                    Logger.getLogger(ConfigureDefaultGlassFishJDK.class.getName()).log(Level.FINEST,"",ioe);
                }
            }
        }
        
    }
    
    static boolean isUnix() {
        return File.separatorChar == '/';
    }    
}
