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

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

public abstract class Constants {

    public final static String GLASSFISH3_BUNDLE = "com.sun.enterprise.jst.server.sunappsrv.glassfishv3prelude";
    public final static String SERVER_GLASSFISH_2_ID = "com.sun.enterprise.jst.server.sunappsrv91";
    public final static String SERVER_PRELUDE_ID = "com.sun.enterprise.jst.server.glassfishv3prelude";
    private static IPath glassFishLocation;

    public static IPath getGlassFishLocation() throws CoreException {
        if (glassFishLocation == null) {
            try {
                URL entry = Platform.getBundle(GLASSFISH3_BUNDLE).getEntry("glassfishv3-prelude/glassfish");
                if (entry == null)
                    throw new CoreException(new Status(IStatus.ERROR, "com.sun.enterprise.jst.server.sunappsrv.configurator",
                            "Glassfish server not found"));
                String file = FileLocator.toFileURL(entry).getFile();
                Path path = new Path(file);
                glassFishLocation = path.removeTrailingSeparator();

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return glassFishLocation;
    }

}