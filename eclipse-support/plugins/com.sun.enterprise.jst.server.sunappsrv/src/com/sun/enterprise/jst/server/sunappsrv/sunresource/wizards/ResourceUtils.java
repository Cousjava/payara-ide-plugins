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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

public class ResourceUtils {
	
	public static final String RESOURCE_FILE_NAME = "sun-resources.xml"; //$NON-NLS-1$
	public static final String SETUP_DIR_NAME = "WebContent/WEB-INF"; //$NON-NLS-1$
	
	private static final String SUN_RESOURCES_XML_HEADER = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + "<!DOCTYPE resources PUBLIC " +  //$NON-NLS-1$ //$NON-NLS-2$
            "\"-//Sun Microsystems, Inc.//DTD Application Server 9.0 Resource Definitions //EN\" " + //$NON-NLS-1$ 
            "\"http://www.sun.com/software/appserver/dtds/sun-resources_1_3.dtd\">\n" + //$NON-NLS-1$
        "<resources>\n"; //$NON-NLS-1$
    private static final String SUN_RESOURCES_XML_FOOTER = "</resources>\n"; //$NON-NLS-1$
    
    public static InputStream appendResource(IFile sunResourcesXml, String fragment) throws IOException, CoreException {
        String sunResourcesBuf = readResourceFile(sunResourcesXml);
        sunResourcesBuf = insertFragment(sunResourcesBuf, fragment);
        return new ByteArrayInputStream(sunResourcesBuf.getBytes());
    }
    
    public static String readResourceFile(IFile sunResourcesXml) throws IOException, CoreException {
        String content = null;
        if(sunResourcesXml.exists()) {
        	InputStream is = null;
        	Reader reader = null;
        	try {
        		IPath location = sunResourcesXml.getLocation();
        		if (location != null) {
        			File resFile = location.toFile();
        			long flen = resFile.length();
        			if(flen > 1000000) {
        				throw new IOException(resFile.getAbsolutePath() + " is too long to update."); //$NON-NLS-1$
        			}
        			int length = (int) (2 * flen + 32);
        			char [] buf = new char[length];
        			is = new BufferedInputStream(sunResourcesXml.getContents());
        			String encoding = sunResourcesXml.getCharset();
        			reader = new InputStreamReader(is, encoding);
        			int max = reader.read(buf);
        			if(max > 0) {
        				content = new String(buf, 0, max);
        			}
        		}
        	} finally {
        		if(is != null) {
        			try { is.close(); } catch(IOException ex) { }
        		}
        		if(reader != null) {
        			try { reader.close(); } catch(IOException ex) { }
        		}
        	}
        }
        return content;
    }
    
	public static String insertFragment(String sunResourcesBuf, String fragment) throws IOException {
        String header = SUN_RESOURCES_XML_HEADER;
        String footer = SUN_RESOURCES_XML_FOOTER;
        boolean insertNewLine = false;
        
        if(sunResourcesBuf != null) {
            int closeIndex = sunResourcesBuf.indexOf("</resources>"); //$NON-NLS-1$
            if(closeIndex == -1) {
                throw new IOException("Malformed XML"); //$NON-NLS-1$
            }
            header = sunResourcesBuf.substring(0, closeIndex);
            footer = sunResourcesBuf.substring(closeIndex);
            
            if(closeIndex > 0 && sunResourcesBuf.charAt(closeIndex-1) != '\n') { //$NON-NLS-1$
                insertNewLine = true;
            }
        }
        
        int length = header.length() + footer.length() + 2;
        if(fragment != null) {
            length += fragment.length();
        }
        
        StringBuilder builder = new StringBuilder(length);
        builder.append(header);
        
        if(insertNewLine) {
            String lineSeparator = System.getProperty("line.separator"); //$NON-NLS-1$
            builder.append(lineSeparator != null ? lineSeparator : "\n"); //$NON-NLS-1$
        }
        
        if(fragment != null) {
            builder.append(fragment);
        }
        
        builder.append(footer);
        return builder.toString();
    }
	
	public static String replaceOrRemove(String originalLine, String pattern, String value) {
		String containsPattern = ".*" + pattern + ".*"; //$NON-NLS-1$ //$NON-NLS-2$
		if ((originalLine != null) && Pattern.matches(containsPattern, originalLine)) {
			return (((value == null) || (value.length() == 0)) ? null : 
				originalLine.replaceAll(pattern, value));
		}
		return originalLine;
	}
}
