/*
* CDDL HEADER START
*
* The contents of this file are subject to the terms of the
* Common Development and Distribution License, Version 1.0 only
* (the "License").  You may not use this file except in compliance
* with the License.
*
* You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
* or http://www.opensolaris.org/os/licensing.
* See the License for the specific language governing permissions
* and limitations under the License.
*
* When distributing Covered Code, include this CDDL HEADER in each
* file and include the License file at usr/src/OPENSOLARIS.LICENSE.
* If applicable, add the following below this CDDL HEADER, with the
* fields enclosed by brackets "[]" replaced with your own identifying
* information: Portions Copyright [yyyy] [name of copyright owner]
*
* CDDL HEADER END
*/
/*
 * Copyright 2007 Sun Microsystems, Inc.  All rights reserved.
* Use is subject to license terms.
*/

package samp.scripts.startmenus;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author root
 */
public class InstallMenus {

    public static void initMenus() {

        String homeDir = System.getProperty("user.home");
        File apps = new File(homeDir, ".local/share/applications");

        if (apps.exists() == false) {
            apps.mkdirs();
        } else {
            /// return;
        }
        File desktop = new File(homeDir, ".local/share/desktop-directories");

        if (desktop.exists() == false) {
            desktop.mkdirs();
        }
        File merged = new File(homeDir, ".config/menus/applications-merged");

        if (merged.exists() == false) {
            merged.mkdirs();
        }
        //resources is same dir as this class:
        copyResource("webstack.directory", desktop);
        copyResource("webstack-logs.directory", desktop);
        copyResource("webstack-sampleapp.directory", desktop);
        copyResource("webstack-start.desktop", apps);
        copyResource("webstack-stop.desktop", apps);
        copyResource("webstack-options.desktop", apps);
        copyResource("webstack-apachelog.desktop", apps);
        copyResource("webstack-mysqllog.desktop", apps);
        copyResource("webstack-uninitialize.desktop", apps);
        copyResource("webstack-createsample.desktop", apps);
        copyResource("webstack-runsample.desktop", apps);
        copyResource("webstack-administermysql.desktop", apps);
        copyResource("webstack-viewgettingstartedguide.desktop", apps);
        copyResource("webstack.menu", merged);

    }

    private static void copyResource(String resourceName, File directory) {
        try {
            URL url = InstallMenus.class.getResource(resourceName);
            String content = readResource(url.openStream(), "UTF-8");

            FileOutputStream fos = new FileOutputStream(new File(directory, resourceName));
            BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(fos, "UTF-8"));
            bw.write(content);
            bw.close();
            fos.close();
        } catch (IOException ex) {
            Logger.getLogger(InstallMenus.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public static String readResource(InputStream is, String encoding) throws IOException {
        // read the config from resource first
        StringBuffer sb = new StringBuffer();
        String lineSep = System.getProperty("line.separator"); //NOI18N
        BufferedReader br = new BufferedReader(new InputStreamReader(is, encoding));
        String line = br.readLine();

        while (line != null) {
            sb.append(line);
            sb.append(lineSep);
            line = br.readLine();
        }

        br.close();

        return sb.toString();
    }
}