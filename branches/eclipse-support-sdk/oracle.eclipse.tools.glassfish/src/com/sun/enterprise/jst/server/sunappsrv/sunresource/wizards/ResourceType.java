package com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards;

public enum ResourceType {

	JDBC_RESOURCE("/resources/jdbc-resource", "jndi-name"), 
	JDBC_CONNECTION_POOL("/resources/jdbc-connection-pool", "name"), 
	JAVA_MAIL("/resources/mail-resource", "jndi-name"), 
	CONNECTOR_RESOURCE("/resources/connector-resource", "jndi-name"),
	CONNECTOR_POOL("/resources/connector-connection-pool", "name"),
	ADMIN_OBJECT_RESOURCE("/resources/admin-object-resource", "jndi-name");
	
	private String defaultKeyName;
	private String defaultPath;
	
	private ResourceType(String defaultPath, String defaultKeyName) {
		this.defaultPath = defaultPath;
		this.defaultKeyName = defaultKeyName;
	}
	
	public String getDefaultPath() {
		return defaultPath;
	}
	
	public String getDefaultKeyName() {
		return defaultKeyName;
	}
}
