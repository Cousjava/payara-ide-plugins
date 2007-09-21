/*
 * OptionsModel.java
 * 
 * Created on Sep 20, 2007, 5:10:58 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package samp.options;

/**
 *
 * @author ludo
 */
public class OptionsModel {
    private boolean startServerWhenSolarisBoots;
    private boolean startServersWhenILogIn;
    private int portNumber;
    private String documentDirectory;
    private int PHPVersion;
    private String PHPErrorReportingLevel;
    private boolean MySQLAllowRemoteAccess;
    private String MySQLPassword;
    private boolean FTPEnable;

    public OptionsModel() {
    }

    public boolean isStartServerWhenSolarisBoots() {
        return startServerWhenSolarisBoots;
    }

    public void setStartServerWhenSolarisBoots(boolean startServerWhenSolarisBoots) {
        this.startServerWhenSolarisBoots = startServerWhenSolarisBoots;
    }

    public boolean isStartServersWhenILogIn() {
        return startServersWhenILogIn;
    }

    public void setStartServersWhenILogIn(boolean startServersWhenILogIn) {
        this.startServersWhenILogIn = startServersWhenILogIn;
    }

    public int getPortNumber() {
        return portNumber;
    }

    public void setPortNumber(int portNumber) {
        this.portNumber = portNumber;
    }

    public String getDocumentDirectory() {
        return documentDirectory;
    }

    public void setDocumentDirectory(String documentDirectory) {
        this.documentDirectory = documentDirectory;
    }

    public int getPHPVersion() {
        return PHPVersion;
    }

    public void setPHPVersion(int PHPVersion) {
        this.PHPVersion = PHPVersion;
    }

    public String getPHPErrorReportingLevel() {
        return PHPErrorReportingLevel;
    }

    public void setPHPErrorReportingLevel(String PHPErrorReportingLevel) {
        this.PHPErrorReportingLevel = PHPErrorReportingLevel;
    }

    public boolean isMySQLAllowRemoteAccess() {
        return MySQLAllowRemoteAccess;
    }

    public void setMySQLAllowRemoteAccess(boolean MySQLAllowRemoteAccess) {
        this.MySQLAllowRemoteAccess = MySQLAllowRemoteAccess;
    }

    public String getMySQLPassword() {
        return MySQLPassword;
    }

    public void setMySQLPassword(String MySQLPassword) {
        this.MySQLPassword = MySQLPassword;
    }

    public boolean isFTPEnable() {
        return FTPEnable;
    }

    public void setFTPEnable(boolean FTPEnable) {
        this.FTPEnable = FTPEnable;
    }
    
    

}
