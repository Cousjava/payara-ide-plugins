package com.sun.enterprise.jst.server.sunappsrv.configreaders;

import java.util.HashMap;
import java.util.Map;

import org.glassfish.tools.ide.server.parser.DomainConfigReader;
import org.glassfish.tools.ide.server.parser.TreeParser.NodeListener;
import org.glassfish.tools.ide.server.parser.TreeParser.Path;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards.ResourceType;

public class ResourcesReader extends NodeListener implements
		DomainConfigReader {

	private String path;
	private String keyName;
	
	private Map<String, String> properties = null;
	private Map<String, Map<String, String>> resourceData = new HashMap<String, Map<String, String>>();
	
	public ResourcesReader(ResourceType type) {
		this(type, type.getDefaultPath(), type.getDefaultKeyName());
	}
	
	public ResourcesReader(ResourceType type, String path, String keyName) {
		this.path = path;
		this.keyName = keyName;
	}

	@Override
	public void readAttributes(String qname, Attributes attributes) throws SAXException {
		properties = new HashMap<String, String>();

		String resourceName = attributes.getValue(keyName);
		properties.put(keyName, resourceName);  

		int attrLen = attributes.getLength();
		for (int i = 0; i < attrLen; i++) {
			String name = attributes.getQName(i);
			String value = attributes.getValue(i);
			if (name != null && name.length() > 0 && value != null && value.length() > 0) {
				properties.put(name, value);
			}
		}
	}

	@Override
	public void readChildren(String qname, Attributes attributes) throws SAXException {
		String propName = qname + "." + attributes.getValue("name"); //$NON-NLS-1$ //$NON-NLS-2$
		properties.put(propName, attributes.getValue("value"));  //$NON-NLS-1$
	}

	@Override
	public void endNode(String qname) throws SAXException {
		String poolName = properties.get(keyName);  
		resourceData.put(poolName, properties);
	}
	
	@Override
	public Path[] getPathsToListen() {
		return new Path[] {new Path(path, this)};
	}

	public Map<String, Map<String, String>> getResourceData() {
		return resourceData;
	}
	
}
