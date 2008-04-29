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


package com.sun.enterprise.jst.server.sunappsrv.sunresource;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * For converting NetBeans DB Urls to their component parts or back.
 * NOTE: This is a copy of the file used in the NB V3 plugin, though it's 
 * in a different package there (org.netbeans.modules.glassfish.javaee.db).
 *
 * @author Peter Williams
 */
public class UrlData {

    private static String DBURL_PATTERN =
            "([^/]*?)//([^:/]*?)(?::([^/]*?)|)/([^;\"]*)(?:;([^\"]*)|)"; // NOI18N

    private static final int DB_PREFIX = 0;
    private static final int DB_HOST = 1;
    private static final int DB_PORT = 2;
    private static final int DB_DATABASE_NAME= 3;
    private static final int DB_SID = 4;
    private static final int NUM_PARTS = 5;
    
    private static Pattern urlPattern = Pattern.compile(DBURL_PATTERN);

    private final String url;
    private final String [] parts;

    public UrlData(String newUrl) {
        url = newUrl;
        parts = new String [NUM_PARTS];
        parseUrl();
    }
    
    public UrlData(final String prefix, final String host, final String port, 
            final String dbname, final String sid) {
        parts = new String [NUM_PARTS];
        parts[DB_PREFIX] = prefix;
        parts[DB_HOST] = host;
        parts[DB_PORT] = port;
        parts[DB_DATABASE_NAME] = dbname;
        parts[DB_SID] = sid;
        url = constructUrl();
    }

    private void parseUrl() {
        Logger.getLogger("glassfish-javaee").log(Level.FINER, "Parsing DB Url: " + url);
        Matcher matcher = urlPattern.matcher(url);
        if(matcher.matches()) {
            for(int i = 1; i <= matcher.groupCount(); i++) {
                String part = matcher.group(i);
                Logger.getLogger("glassfish-javaee").log(Level.FINER, "Matched " + part + " at index " + i);
                parts[i-1] = part;
            }
        } else {
            Logger.getLogger("glassfish-javaee").log(Level.FINE, "Url parsing failed for " + url);
        }
    }
    
    private String constructUrl() {
        StringBuilder builder = new StringBuilder(256);
        builder.append(parts[DB_PREFIX]);
        builder.append("//"); // NOI18N
        builder.append(parts[DB_HOST]);
        String port = parts[DB_PORT];
        if(port != null && port.length() > 0) {
            builder.append(':'); // NOI18N
            builder.append(port);
        }
        String dbname = parts[DB_DATABASE_NAME];
        if(dbname != null && dbname.length() > 0) {
            builder.append('/'); // NOI18N
            builder.append(dbname);
        }
        String sid = parts[DB_SID];
        if(sid != null && sid.length() > 0) {
            builder.append(';'); // NOI18N
            builder.append(sid);
        }
        return builder.toString();
    }
    
    public String getUrl() {
        return url;
    }
    
    public String getPrefix() {
        return parts[DB_PREFIX];
    }

    public String getHostName() {
        return parts[DB_HOST];
    }

    public String getPort() {
        return parts[DB_PORT];
    }

    public String getDatabaseName() {
        return parts[DB_DATABASE_NAME];
    }

    public String getSid() {
        return parts[DB_SID];
    }

}
