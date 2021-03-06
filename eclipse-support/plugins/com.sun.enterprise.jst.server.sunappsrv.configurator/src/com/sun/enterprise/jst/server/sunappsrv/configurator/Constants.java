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
package com.sun.enterprise.jst.server.sunappsrv.configurator;

public abstract class Constants {

	public final static String SERVER_GLASSFISH_2_ID = "com.sun.enterprise.jst.server.sunappsrv91"; //$NON-NLS-1$
    public final static String SERVER_PRELUDE_ID = "com.sun.enterprise.jst.server.glassfishv3prelude"; //$NON-NLS-1$
    public final static String SERVER_GLASSFISH_V3_ID = "com.sun.enterprise.jst.server.sunappsrv92"; //$NON-NLS-1$
	public final static String DERBY_SAMPLE_ID = "com.sun.enterprise.jst.server.derbysample"; //$NON-NLS-1$

	public static final String GLASSFISHV2_1 = "glassfishv2.1";
	
	public final static String ADMIN_PORT = "admin.port"; //$NON-NLS-1$
	public final static String INSTANCE_PORT = "instance.port"; //$NON-NLS-1$
	public final static String HTTPS_PORT = "https.port"; //$NON-NLS-1$
	public final static String ORB_PORT = "orb.port"; //$NON-NLS-1$
	public final static String IMQ_PORT = "imq.port"; //$NON-NLS-1$
	public final static String GLASSFISH_DIR = "glassfish_dir"; //$NON-NLS-1$
	public final static String ADMIN_USERNAME = "admin_username"; //$NON-NLS-1$
	public final static String ADMIN_PASSWORD = "admin_password"; //$NON-NLS-1$
	public final static String DOMAIN_NAME = "domain_name"; //$NON-NLS-1$
	public final static String DOMAIN_DIR = "domain.dir"; //$NON-NLS-1$

	// V2 configuration constants
	public static final int V2_INSTANCE_PORT = 8082;
    public static final int V2_ADMIN_PORT = 4849;
    public static final int V2_HTTPS_PORT = 8182;
    public static final int V2_IMQ_PORT = 7677;
    public static final int V2_ORB_PORT = 3701;
	public static final String V2_USER = "admin"; //$NON-NLS-1$
	public static final String V2_PASS = "adminadmin"; //$NON-NLS-1$

    // V3 Prelude configuration constants
    public static final int V3PRELUDE_ADMIN_PORT = 4850;
    public static final int V3PRELUDE_HTTP_PORT = 8083;

    // V3 Prelude configuration constants
    public static final int V3_ADMIN_PORT = 4860;
    public static final int V3_HTTP_PORT = 8084;

}
