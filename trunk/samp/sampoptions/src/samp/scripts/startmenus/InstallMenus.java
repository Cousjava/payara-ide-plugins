/*
 * InstallMenus.java
 *
 * Created on Oct 4, 2007, 4:47:30 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
        System.out.println("home dir" + homeDir);
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
        copyResource("webstack-start.desktop", apps);
        copyResource("webstack-stop.desktop", apps);
        copyResource("webstack-configure.desktop", apps);
        copyResource("webstack.menu", merged);

        copyResource("webstack.directory", desktop);
    }

    private static void copyResource(String resourceName, File directory) {
        try {
            URL url = InstallMenus.class.getResource(resourceName);
            String content = readResource(url.openStream(), "UTF-8");
System.out.println("contnet"+content);
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