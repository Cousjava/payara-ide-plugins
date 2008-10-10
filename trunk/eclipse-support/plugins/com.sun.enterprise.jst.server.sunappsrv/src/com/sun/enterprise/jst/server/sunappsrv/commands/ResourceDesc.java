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

package com.sun.enterprise.jst.server.sunappsrv.commands;


/**
 *
 * @author Peter Williams
 */
public class ResourceDesc implements Comparable<ResourceDesc> {
    
    private final String name;
    private final String cmdSuffix;
    
    public ResourceDesc(final String name, final String cmdSuffix) {
        this.name = name;
        this.cmdSuffix = cmdSuffix;
    }

    public String getName() {
        return name;
    }

    public String getCommandSuffix() {
        return cmdSuffix;
    }

    public int compareTo(ResourceDesc o) {
        int result = name.compareTo(o.name);
        if(result == 0) {
            result = cmdSuffix.compareTo(o.cmdSuffix);
        }
        return result;
    }
    
}
