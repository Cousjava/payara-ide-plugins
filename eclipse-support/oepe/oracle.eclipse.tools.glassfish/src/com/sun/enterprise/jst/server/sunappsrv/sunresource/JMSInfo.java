/*
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Sun Microsystems
 *     Oracle
 */


package com.sun.enterprise.jst.server.sunappsrv.sunresource;

/**
 * @author Nitya Doraisamy
 *
 */
public class JMSInfo {
	
	public static final String QUEUE = "javax.jms.Queue"; //$NON-NLS-1$
    public static final String TOPIC = "javax.jms.Topic"; //$NON-NLS-1$
    public static final String QUEUE_CONNECTION = "javax.jms.QueueConnectionFactory"; //$NON-NLS-1$ 
    public static final String TOPIC_CONNECTION = "javax.jms.TopicConnectionFactory"; //$NON-NLS-1$
    public static final String CONNECTION_FACTORY = "javax.jms.ConnectionFactory"; //$NON-NLS-1$
    
	private String jndiName;
	private String resourceType;
	private boolean isConnector;
	
	public JMSInfo() {
        
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
	 * @param resourceType the resourceType to set
	 */
	public void setResourceType(String resourceType) {
		this.resourceType = resourceType;
	}

	/**
	 * @return the resourceType
	 */
	public String getResourceType() {
		return resourceType;
	}

	/**
	 * @return the isConnector
	 */
	public boolean isConnector() {
		if(resourceType.equals(QUEUE) || resourceType.equals(QUEUE)){
			isConnector = false;
		} else { 
			isConnector = true;
		}
		return isConnector;
	}
	
}
