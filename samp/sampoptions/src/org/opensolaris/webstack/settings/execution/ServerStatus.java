/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.opensolaris.webstack.settings.execution;

/**
 *
 * @author ludo
 */
public final class ServerStatus {
    public enum Status  {online, disabled, maintenance,};
    public boolean apacheRunning=false;
    public boolean mySqlRunning=false;
}
