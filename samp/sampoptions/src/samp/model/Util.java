/*
 * Util.java
 *
 * Created on Sep 27, 2007, 8:30:29 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.model;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ludo
 */
public class Util {





    private void updateFile(File target, String content) throws IOException {
        try {
            OutputStream os = new FileOutputStream(target);
            BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(os, "UTF-8"));
            bw.write(content);
            bw.close();
        } finally {
        }
    }

    public static String readAndReplace(InputStream is, String oldStartLine, String newLine) throws IOException {
        // read the config from resource first
        StringBuffer sb = new StringBuffer();
        String lineSep = System.getProperty("line.separator"); //NOI18N
        BufferedReader br = new BufferedReader(new InputStreamReader(is, "UTF-8"));
        String line = br.readLine();


        while (line != null) {
            if (line.startsWith(oldStartLine)) {
                line = newLine;
            }

            sb.append(line);
            sb.append(lineSep);
            line = br.readLine();
        }

        br.close();

        return sb.toString();
    }
    
        public static String getValue(File confFile, String key) {
        FileInputStream fis = null;
        BufferedReader br =null;
        try {
            fis = new FileInputStream(confFile);
            // read the config from resource first
            br = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
            String line = br.readLine();


            while (line != null) {
                if (line.startsWith(key)) {
                    System.out.println(" line is "+line);
                    return line.substring(key.length(), line.length());
                }

                line = br.readLine();
            }



        } catch (Exception ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            try {
                if (br!=null) br.close();
            } catch (IOException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return "80";
    }
}