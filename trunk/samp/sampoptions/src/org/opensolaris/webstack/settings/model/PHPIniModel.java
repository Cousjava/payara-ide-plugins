/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opensolaris.webstack.settings.model;

import java.io.File;

/**
 *
 * @author ludo
 */
public class PHPIniModel extends Model {

    int errorreportingKey = -1;
    int xdebugKey =  -1;
    int zend_extensionKey =  -1;
    int xdebugremote_enableKey =  -1;
    int xdebugremote_modeKey =  -1;
    int xdebugremote_hostKey =  -1;

    public PHPIniModel() {
        super(new File(Environment.getPhpini()));
    }

    @Override
    public void lineAddedCallBack(String line, int lineNumber) {
        if (line.startsWith("error_reporting")) {
            errorreportingKey = lineNumber;
        } else   if (line.startsWith("error_reporting")) {
            errorreportingKey = lineNumber;
        } else   if (line.startsWith("zend_extension")) {
            zend_extensionKey = lineNumber;
        } else   if (line.startsWith("xdebug.remote_enableKey")) {
            xdebugremote_enableKey = lineNumber;
        } else   if (line.startsWith("xdebug.remote_modeKey")) {
            xdebugremote_modeKey = lineNumber;
        } else   if (line.startsWith("xdebug.remote_hostKey")) {
            xdebugremote_hostKey = lineNumber;
        } 
            

    }
    
    public boolean isDebugMode(){
        return (xdebugremote_enableKey!=-1);// this key is present in the file
    }
    
    public void setDebugMode(boolean debugmode){
        if (debugmode){
            if (zend_extensionKey!=-1){
                    content.set(zend_extensionKey, "zend_extension=/Applications/MAMP/bin/php5/lib/php/extensions/no-debug-non-zts-20050922/xdebug.so");                
            }
            else {
                   content.add("zend_extension=/Applications/MAMP/bin/php5/lib/php/extensions/no-debug-non-zts-20050922/xdebug.so");                
                
            }
            if (xdebugremote_enableKey!=-1){
                    content.set(xdebugremote_enableKey, "xdebug.remote_enable=1111");                
            }
            else {
                   content.add("xdebug.remote_enable=1222");                
                
            }            
        }else{
            content.remove(xdebugremote_hostKey);
            content.remove(xdebugremote_modeKey);
            content.remove(xdebugremote_enableKey);
            content.remove(zend_extensionKey);

        }
        
    }
}
/*
 * 
 * [xdebug]
zend_extension=/Applications/MAMP/bin/php5/lib/php/extensions/no-debug-non-zts-20050922/xdebug.so
xdebug.remote_enable=1
xdebug.remote_handler=dbgp
xdebug.remote_mode=req
xdebug.remote_host=localhost
 * */