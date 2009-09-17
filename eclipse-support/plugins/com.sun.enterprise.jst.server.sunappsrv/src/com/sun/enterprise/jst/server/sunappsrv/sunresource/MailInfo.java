package com.sun.enterprise.jst.server.sunappsrv.sunresource;

public class MailInfo {

	private String jndiName;
	private String mailHost;
	private String mailFrom;
	private String mailUser;
		
	public MailInfo() {
        
    }

	/**
	 * @param jndiName the jndiName to set
	 */
	public void setJndiName(String jndiName) {
		this.jndiName = jndiName;
	}

	/**
	 * @return the jndiName
	 */
	public String getJndiName() {
		return jndiName;
	}

	/**
	 * @param mailHost the mailHost to set
	 */
	public void setMailHost(String mailHost) {
		this.mailHost = mailHost;
	}

	/**
	 * @return the mailHost
	 */
	public String getMailHost() {
		return mailHost;
	}

	/**
	 * @param mailFrom the mailFrom to set
	 */
	public void setMailFrom(String mailFrom) {
		this.mailFrom = mailFrom;
	}

	/**
	 * @return the mailFrom
	 */
	public String getMailFrom() {
		return mailFrom;
	}

	/**
	 * @param mailUser the mailUser to set
	 */
	public void setMailUser(String mailUser) {
		this.mailUser = mailUser;
	}

	/**
	 * @return the mailUser
	 */
	public String getMailUser() {
		return mailUser;
	}
	
}
