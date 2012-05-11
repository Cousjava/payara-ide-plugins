package com.sun.enterprise.jst.server.sunappsrv.spi;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser.NodeReader;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser.Path;

public class JvmConfigReader extends NodeReader implements
		DomainConfigReader {
	
	private String serverName;
	
	private ArrayList<String> optList = new ArrayList<String>();
	private HashMap<String, String> propMap = new HashMap<String, String>();
	private boolean isMonitoringEnabled = false;
	
	private String serverConfigName;
    private boolean readJvmConfig = false;
	
	public JvmConfigReader(String serverName) {
		this.serverName = serverName;
	}

	public TreeParser.NodeReader getServerFinder() {
        return new TreeParser.NodeReader() {
            @Override
            public void readAttributes(String qname, Attributes attributes) throws SAXException {
//                <server lb-weight="100" name="server" config-ref="server-config">
                if (serverConfigName == null || serverConfigName.length() == 0) {
                    if (serverName.equals(attributes.getValue("name"))) {        // NOI18N
                        serverConfigName = attributes.getValue("config-ref");   // NOI18N
                        //Logger.getLogger("glassfish").finer("DOMAIN.XML: Server profile defined by " + serverConfigName); // NOI18N
                    }
                }
            }
        };
    }

    public TreeParser.NodeReader getConfigFinder() {
        return new TreeParser.NodeReader() {
            @Override
            public void readAttributes(String qname, Attributes attributes) throws SAXException {
//                <config name="server-config" dynamic-reconfiguration-enabled="true">
                if (serverConfigName != null && serverConfigName.equals(attributes.getValue("name"))) { // NOI18N
                    readJvmConfig = true;
                    //Logger.getLogger("glassfish").finer("DOMAIN.XML: Reading JVM options from server profile " + serverConfigName); // NOI18N
                }
            }

            @Override
            public void endNode(String qname) throws SAXException {
                readJvmConfig = false;
            }
        };
    }

    @Override
    public void readAttributes(String qname, Attributes attributes) throws SAXException {
//        <java-config
//            classpath-prefix="CP-PREFIX"
//            classpath-suffix="CP-SUFFIX"
//            debug-enabled="false"
//            debug-options="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=9009"
//            env-classpath-ignored="false"
//            java-home="${com.sun.aas.javaRoot}"
//            javac-options="-g"
//            native-library-path-prefix="NATIVE-LIB-PREFIX"
//            native-library-path-suffix="NATIVE-LIB-SUFFIX"
//            rmic-options="-iiop -poa -alwaysgenerate -keepgenerated -g"
//            server-classpath="SERVER-CLASSPATH"
//            system-classpath="SYSTEM-CLASSPATH">
        if (readJvmConfig) {
            int attrLen = attributes.getLength();
            for (int i = 0; i < attrLen; i++) {
                String name = attributes.getLocalName(i);
                String value = attributes.getValue(i);
                if (name != null && name.length() > 0 && value != null && value.length() > 0) {
                    propMap.put(name, value);
                }
            }
        }
    }

    @Override
    public void readCData(String qname, char[] ch, int start, int length) throws SAXException {
//        <jvm-options>-client</jvm-options>
//        <jvm-options>-Djava.endorsed.dirs=${com.sun.aas.installRoot}/lib/endorsed</jvm-options>
        if (readJvmConfig) {
            String option = new String(ch, start, length);
            optList.add(option);
        }
    }
    
    public TreeParser.NodeReader getMonitoringFinder() {
    	return new TreeParser.NodeReader() {
    		@Override
    		public void readAttributes(String qname, Attributes attributes) throws SAXException {
    			//                <monitoring-service [monitoring-enabled="false"] 
    			if (readJvmConfig) {
    				isMonitoringEnabled = !"false".equals(attributes.getValue("monitoring-enabled"));
//    				if (monitoringAgent.exists()) {
//    					if (!"false".equals(attributes.getValue("monitoring-enabled"))) {  // NOI18N
//    						//optList.add("-javaagent:"+Utils.quote(monitoringAgent.getAbsolutePath())+"=unsafe=true,noServer=true"); // NOI18N
//    						isMonitoringEnabled = true;
//    					}
//    				}
    			}
    		}
    	};
    }
    
    @Override
    public Path[] getPathsToListen() {
    	return new Path[] {new Path("/domain/servers/server", getServerFinder()),
    			new Path("/domain/configs/config", getConfigFinder()),
    			new Path("/domain/configs/config/java-config", this),
    			new Path("/domain/configs/config/monitoring-service", getMonitoringFinder())};
    }

	public List<String> getOptList() {
		return optList;
	}
	
	public HashMap<String, String> getPropMap() {
		return propMap;
	}

	public boolean isMonitoringEnabled() {
		return isMonitoringEnabled;
	}

}
