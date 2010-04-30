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

package com.sun.enterprise.jst.server.sunappsrv.log;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
/**
 * author: Peter Williams
 */
public class V3LogFilter {
	
    private final Locale logLocale = getLogLocale();
    private final String logBundleName = getLogBundle();
    private final String localizedWarning = getLocalized(Level.WARNING.getName());
    private final String localizedSevere = getLocalized(Level.SEVERE.getName());
    private final Map<String, String> localizedLevels = getLevelMap();
    
    private Locale getLogLocale() {
        // XXX detect and use server language/country/variant instead of IDE's.
        String language = System.getProperty("user.language");
        if(language != null) {
            return new Locale(language, System.getProperty("user.country", ""), System.getProperty("user.variant", ""));
        }
        return Locale.getDefault();
    }
    
    private String getLogBundle() {
        return Level.INFO.getResourceBundleName();
    }
    
    private String getLocalized(String text) {
        ResourceBundle bundle = ResourceBundle.getBundle(logBundleName, logLocale);
        String localized = bundle.getString(text);
        return localized != null ? localized : text;
    }

    public  Map<String, String> getLevelMap() {
        Map<String, String> levelMap = new HashMap<String, String>();
        for(Level l: new Level [] { Level.ALL, Level.CONFIG, Level.FINE,
                Level.FINER, Level.FINEST, Level.INFO, Level.SEVERE, Level.WARNING } ) {
            String name = l.getName();
            levelMap.put(name, getLocalized(name));
        }
        return levelMap;
    }
    
    public String getLocalizedLevel(String level) {
        String localizedLevel = localizedLevels.get(level);
        return localizedLevel != null ? localizedLevel : level;
    }	
	
    public static interface Filter {
        
        public String process(char c);
        
    }
    
    public static abstract class StateFilter implements Filter {
        
        protected String message;
        
        protected int state;
        protected StringBuilder msg;
        
        StateFilter() {
            state = 0;
            msg = new StringBuilder(128);
        }
        
        protected void reset() {
            message = "";
        }
        
        public abstract String process(char c);
        
    }
    
    public static final class StreamFilter extends StateFilter {

        private static final Pattern messagePattern = Pattern.compile("([\\p{Lu}]{0,16}?):|([^\\r\\n]{0,24}?\\d\\d?:\\d\\d?:\\d\\d?)");
        
        private String line;
        
        public StreamFilter() {
            reset();
        }

        @Override
        protected void reset() {
            super.reset();
            line = "";
        }

        /**
         * GlassFish server log format, when read from process stream:
         *
         * Aug 13, 2008 3:01:49 PM com.sun.enterprise.glassfish.bootstrap.ASMain main
         * INFO: Launching GlassFish on Apache Felix OSGi platform
         * Aug 13, 2008 3:01:50 PM com.sun.enterprise.glassfish.bootstrap.ASMainHelper setUpOSGiCache
         * INFO: Removing Felix cache profile dir /space/tools/v3Aug7/domains/domain1/.felix/gf left from a previous run
         * 
         * Welcome to Felix.
         * =================
         * 
         * Aug 13, 2008 3:01:51 PM HK2Main start
         * INFO: contextRootDir = /space/tools/v3Aug7/modules
         * ...
         * Aug 13, 2008 3:02:14 PM
         * SEVERE: Exception in thread "pool-6-thread-1"
         * Aug 13, 2008 3:02:14 PM org.glassfish.scripting.rails.RailsDeployer load
         * INFO: Loading application RailsGFV3 at /RailsGFV3
         * Aug 13, 2008 3:02:14 PM
         * SEVERE: /...absolute.path.../connection_specification.rb:232:in `establish_connection':
         *
         * !PW FIXME This parser should be checked for I18N stability.
         */
        public String process(char c) {
            String result = null;

            if(c == '\n') {
                if(msg.length() > 0) {
                    msg.append(c);
                    line = msg.toString();
                    msg.setLength(0);

                    Matcher matcher = messagePattern.matcher(line);
                    if(matcher.find() && matcher.start() == 0 && matcher.groupCount() > 1 && matcher.group(2) != null) {
                        result = null;
                    } else {
                        result = line;
                    }
                }
            } else if(c != '\r') {
                msg.append(c);
            }

            return result;
        }

    }

    public static final class LogFileFilter extends StateFilter {
        
        private String time;
        private String type;
        private String version;
        private String classinfo;
        private String threadinfo;
        private boolean multiline;
        private final Map<String, String> typeMap;

        public LogFileFilter(Map<String, String> typeMap) {
            this.typeMap = typeMap;
            reset();
        }

        @Override
        protected void reset() {
            super.reset();
            time = type = version = classinfo = threadinfo = "";
            multiline = false;
        }
        
        private String getLocalizedType(String type) {
            String localizedType = typeMap.get(type);
            return localizedType != null ? localizedType : type;
        }

        /**
         * GlassFish server log entry format (unformatted), when read from file:
         *
         * [#|
         *    2008-07-20T16:59:11.738-0700|
         *    INFO|
         *    GlassFish10.0|
         *    org.jvnet.hk2.osgiadapter|
         *    _ThreadID=11;_ThreadName=Thread-6;org.glassfish.admin.config-api [1794];|
         *    Started bundle org.glassfish.admin.config-api [1794]
         * |#]
         *
         * !PW FIXME This parser should be checked for I18N stability.
         */
        public String process(char c) {
            String result = null;

            switch(state) {
                case 0:
                    if(c == '[') {
                        state = 1;
                    } else {
                        if(c == '\n') {
                            if(msg.length() > 0) {
                                msg.append(c);
                                result = msg.toString();
                                msg.setLength(0);
                            }
                        } else if(c != '\r') {
                            msg.append(c);
                        }
                    }
                    break;
                case 1:
                    if(c == '#') {
                        state = 2;
                    } else {
                        state = 0;
                        if(c == '\n') {
                            if(msg.length() > 0) {
                                msg.append(c);
                                result = msg.toString();
                                msg.setLength(0);
                            }
                        } else if(c != '\r') {
                            msg.append('[');
                            msg.append(c);
                        }
                    }
                    break;
                case 2:
                    if(c == '|') {
                        state = 3;
                        msg.setLength(0);
                    } else {
                        if(c == '\n') {
                            if(msg.length() > 0) {
                                msg.append(c);
                                result = msg.toString();
                                msg.setLength(0);
                            }
                        } else if(c != '\r') {
                            state = 0;
                            msg.append('[');
                            msg.append('#');
                            msg.append(c);
                        }
                    }
                    break;
                case 3:
                    if(c == '|') {
                        state = 4;
                        time = msg.toString();
                        msg.setLength(0);
                    } else {
                        msg.append(c);
                    }
                    break;
                case 4:
                    if(c == '|') {
                        state = 5;
                        type = getLocalizedType(msg.toString());
                        msg.setLength(0);
                    } else {
                        msg.append(c);
                    }
                    break;
                case 5:
                    if(c == '|') {
                        state = 6;
                        version = msg.toString();
                        msg.setLength(0);
                    } else {
                        msg.append(c);
                    }
                    break;
                case 6:
                    if(c == '|') {
                        state = 7;
                        classinfo = msg.toString();
                        msg.setLength(0);
                    } else {
                        msg.append(c);
                    }
                    break;
                case 7:
                    if(c == '|') {
                        state = 8;
                        threadinfo = msg.toString();
                        msg.setLength(0);
                    } else {
                        msg.append(c);
                    }
                    break;
                case 8:
                    if(c == '|') {
                        state = 9;
                        message = msg.toString();
                    } else if(c == '\n') {
                        if(msg.length() > 0) { // suppress blank lines in multiline messages
                            msg.append('\n');
                            result = !multiline ? type + ": " + msg.toString() : msg.toString();
                            multiline = true;
                            msg.setLength(0);
                        }
                    } else if(c != '\r') {
                        msg.append(c);
                    }
                    break;
                case 9:
                    if(c == '#') {
                        state = 10;
                    } else {
                        state = 8;
                        msg.append('|');
                        msg.append(c);
                    }
                    break;
                case 10:
                    if(c == ']') {
                        state = 0;
                        msg.setLength(0);
                        result = (multiline ? message : type + ": " + message) + '\n';
                        reset();
                    } else {
                        state = 8;
                        msg.append('|');
                        msg.append('#');
                        msg.append(c);
                    }
                    break;
            }
            return result;
        }
    }
    
}
