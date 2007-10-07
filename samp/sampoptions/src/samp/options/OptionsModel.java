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
