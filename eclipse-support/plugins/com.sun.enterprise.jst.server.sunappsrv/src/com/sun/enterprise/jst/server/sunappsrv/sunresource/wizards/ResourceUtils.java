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
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
        "<!DOCTYPE resources PUBLIC " + 
            "\"-//Sun Microsystems, Inc.//DTD Application Server 9.0 Resource Definitions //EN\" " + 
            "\"http://www.sun.com/software/appserver/dtds/sun-resources_1_3.dtd\">\n" +
        "<resources>\n";
    private static final String SUN_RESOURCES_XML_FOOTER =
        "</resources>\n";
    
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
        				throw new IOException(resFile.getAbsolutePath() + " is too long to update.");
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
            int closeIndex = sunResourcesBuf.indexOf("</resources>");
            if(closeIndex == -1) {
                throw new IOException("Malformed XML");
            }
            header = sunResourcesBuf.substring(0, closeIndex);
            footer = sunResourcesBuf.substring(closeIndex);
            
            if(closeIndex > 0 && sunResourcesBuf.charAt(closeIndex-1) != '\n') {
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
            String lineSeparator = System.getProperty("line.separator");
            builder.append(lineSeparator != null ? lineSeparator : "\n");
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
