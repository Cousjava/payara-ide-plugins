/*DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.

The contents of this file are subject to the terms of either the GNU
General Public License Version 2 only ("GPL") or the Common Development
and Distribution License("CDDL") (collectively, the "License").  You
may not use this file except in compliance with the License. You can obtain
a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
language governing permissions and limitations under the License.

When distributing the software, include this License Header Notice in each
file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
Sun designates this particular file as subject to the "Classpath" exception
as provided by Sun in the GPL Version 2 section of the License file that
accompanied this code.  If applicable, add the following below the License
Header, with the fields enclosed by brackets [] replaced by your own
identifying information: "Portions Copyrighted [year]
[name of copyright owner]"

Contributor(s):

If you wish your version of this file to be governed by only the CDDL or
only the GPL Version 2, indicate your decision by adding "[Contributor]
elects to include this software in this distribution under the [CDDL or GPL
Version 2] license."  If you don't indicate a single choice of license, a
recipient has the option to distribute your version of this file under
either the CDDL, the GPL Version 2 or to extend the choice of license to
its licensees as provided above.  However, if you add GPL Version 2 code
and therefore, elected the GPL Version 2 license, then the option applies
only if the new code is made subject to such option by the copyright
holder.
 */
package com.sun.enterprise.jst.server.sunappsrv.configurator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.jst.server.generic.internal.core.util.FileUtil;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IRuntimeWorkingCopy;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.IServerType;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.ServerCore;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;

@SuppressWarnings("restriction")
public class V3Configurator {
    
    //either Constants.SERVER_PRELUDE_ID or Constants.SERVER_GLASSFISH_V3_ID
    private String serverID;

    //for v3: "glassfishv3", for v3 prelude: "glassfishv3-prelude"
    private String serverRootDirName;
    
    
    int adminPort;

    int HTTPPort;

    /**
     * @param serverID
     *            either Constants.SERVER_PRELUDE_ID or Constants.SERVER_GLASSFISH_V3_ID
     * @param serverRootDirName
     *            : for v3: "glassfishv3", for v3 prelude: "glassfishv3-prelude"
     * @param adminPort
     * @param HTTPPort
     */
    public V3Configurator(String serverID, String serverRootDirName, int adminPort, int HTTPPort) {
        this.serverID = serverID;
        this.serverRootDirName = serverRootDirName;
        this.HTTPPort = HTTPPort;
        this.adminPort = adminPort;
        

    }

	public String configure(IProgressMonitor progressMonitor, String sampleDB) throws CoreException {
		progressMonitor.subTask(Messages.CreatingGlassfishv3Configuration);
		String glassfishLocation = getGlassfishLocation();
		String domainXml = null;
		try {
			IServerType st = ServerCore.findServerType(serverID);// v3
			IRuntime runtime = createRuntime(glassfishLocation);
			IServer[] servers = ServerCore.getServers();

			for (IServer server : servers) {
				if (server.getRuntime() == null) {
					server.delete();
				}
				if (runtime != null && server != null && runtime.equals(server.getRuntime())) {
					Activator.logMessage("found an existing server at " + runtime.getLocation().toOSString(), null, IStatus.INFO);
					String existingDomainLocation = runtime.getLocation().append("domains").	//$NON-NLS-1$ 
						append("domain1").append("config").append("domain.xml").toOSString(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				    File existingDomainXml = new File(existingDomainLocation);
					Activator.logMessage("testing for existing domain.xml at " + existingDomainLocation + ", exists = " + existingDomainXml.exists(), null, IStatus.INFO);
					if (existingDomainXml.exists()) {
						return existingDomainLocation;
					}
					return null;
				}
			}

			IServerWorkingCopy wc = st.createServer(null, null, runtime, null);
			wc.setName(MessageFormat.format(Messages.Bundled, runtime.getName()));

			SunAppServer sunAppServer = (SunAppServer) wc.getAdapter(SunAppServer.class);

			String domainLocation = Platform.getLocation().append(".metadata").append(".plugins").append( //$NON-NLS-1$ //$NON-NLS-2$
					serverID).toOSString();

		     File destDir = new File(domainLocation);
            if (destDir.exists() == false) {//only if the domain is not already there
                copyDomain(domainLocation);
            }

			domainXml = new Path(domainLocation).append("domain1").append("config").append("domain.xml").toOSString(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			setPortsForDomain(domainXml, FreePortManager.getAvailablePort(HTTPPort), FreePortManager.getAvailablePort(adminPort));

			Activator.logMessage("domain.xml location is = " + domainXml, null, IStatus.INFO);
			Map<String, String> configuration = sunAppServer.getProps();
			configuration.put(SunAppServer.DOMAINDIR, domainLocation);
	        configuration.put(SunAppServer.SAMPLEDBDIR, sampleDB);
			sunAppServer.setServerInstanceProperties(configuration);

			wc.save(true, null);

		} catch (CoreException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
					Messages.ErrorInStartupConfig, e.getMessage()), e), e.getMessage(), Messages.EXCEPTION_OCCURRED);
		}

		return domainXml;
	}

	private String getGlassfishLocation() {
		String glassfishLocation = null;

			try {
				// Get the eclipse installation location and from it, gf
				// installation directory.
				glassfishLocation = new Path(Platform.getInstallLocation().getURL().getFile()).toPortableString()
						+ serverRootDirName
                    + "/glassfish"; //$NON-NLS-1$

				Activator.getDefault().getLog().log(
						new Status(IStatus.INFO, Activator.PLUGIN_ID, "glassfishV3Loc =" + glassfishLocation)); //$NON-NLS-1$
				return glassfishLocation;
			} catch (Exception e) {
				Activator
						.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
								Messages.ErrorInStartupConfig, e.getMessage()), e), e.getMessage(),
								Messages.EXCEPTION_OCCURRED);
			}
		
		return glassfishLocation;
	}

	@SuppressWarnings("unchecked")
	private IRuntime createRuntime(String glassfishLocation) {
		try {
			IServerType st = ServerCore.findServerType(serverID);
			IRuntime[] runtimes = ServerCore.getRuntimes();
			for (IRuntime runtime : runtimes) {
				if (runtime != null && runtime.getRuntimeType().equals(st.getRuntimeType())) {
					return runtime;
				}
			}

			IRuntimeWorkingCopy wc;
			wc = st.getRuntimeType().createRuntime(null, null);

			GenericServerRuntime gRun = (GenericServerRuntime) wc.loadAdapter(GenericServerRuntime.class,
					new NullProgressMonitor());

			HashMap map = new HashMap();

			map.put(SunAppServer.ROOTDIR, glassfishLocation);
			gRun.setServerDefinitionId(gRun.getRuntime().getRuntimeType().getId());
			gRun.setServerInstanceProperties(map);

			wc.setLocation(new Path(glassfishLocation));
			return wc.save(true, null);
		} catch (CoreException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
					Messages.ErrorInStartupConfig, e.getMessage()), e), e.getMessage(), Messages.EXCEPTION_OCCURRED);
		}
		return null;
	}

	private void copyDomain(String toDir) throws CoreException {
		String srcDir = getDomainsDir();
		File destDir = new File(toDir);
		if (destDir.exists()) {
			throw new CoreException(new Status(IStatus.ERROR, "com.sun.enterprise.jst.server.sunappsrv.configurator", //$NON-NLS-1$
					Messages.DomainDestinationDirectoryExists));
		}
		destDir.mkdirs();
		copyDir(new File(srcDir), destDir);

	}

	private void copyDir(File srcDir, File destDir) throws CoreException {
		if (srcDir.isDirectory()) {
			File[] listFiles = srcDir.listFiles();
			for (File file : listFiles) {
				File newFile = new File(destDir, file.getName());
				if (file.isDirectory()) {
					newFile.mkdir();
					copyDir(file, newFile);
				} else {
					FileInputStream fis;
					FileOutputStream fos;
					try {
						fis = new FileInputStream(file);
						fos = new FileOutputStream(newFile);
						FileUtil.copy(fis, fos);
						fis.close();
						fos.close();
					} catch (Exception e) {
						Status status = new Status(IStatus.ERROR,
								"com.sun.enterprise.jst.server.sunappsrv.configurator", MessageFormat.format( //$NON-NLS-1$
										Messages.FileCopyingFailed, e), e);
						Activator.getDefault().getLog().log(status);
						throw new CoreException(status);
					}
				}
			}
		}
	}

	private String getDomainsDir() throws CoreException {
		return new Path(getGlassfishLocation()).append("domains").toOSString(); //$NON-NLS-1$
	}

	private void setPortsForDomain(String domainXml, int http, int admin) throws CoreException {
		try {
			DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
			domFactory.setNamespaceAware(true);
			DocumentBuilder builder = domFactory.newDocumentBuilder();
			Document doc = builder.parse(new File(domainXml));

			XPathFactory factory = XPathFactory.newInstance();
			XPath xpath = factory.newXPath();
			XPathExpression expr = xpath.compile("//http-listener[@id='http-listener-1']"); //$NON-NLS-1$
			Node node = (Node) expr.evaluate(doc, XPathConstants.NODE);
			if (node == null) { //gf v3 has different config: network-listener
                expr = xpath.compile("//network-listener[@name='http-listener-1']"); //$NON-NLS-1$
                node = (Node) expr.evaluate(doc, XPathConstants.NODE);
            }
			Node port = node.getAttributes().getNamedItem("port"); //$NON-NLS-1$
			port.setNodeValue("" + http); //$NON-NLS-1$

			expr = xpath.compile("//http-listener[@id='admin-listener']"); //$NON-NLS-1$
			node = (Node) expr.evaluate(doc, XPathConstants.NODE);
	         if (node == null) {//gf v3 has different config: network-listener
                expr = xpath.compile("//network-listener[@name='admin-listener']"); //$NON-NLS-1$
                node = (Node) expr.evaluate(doc, XPathConstants.NODE);
                
                //for gf v3, on mac and with jdk 6 installed, we can optimized by running in 32 bits instead of 64: much faster
                //  <jvm-options>-d32</jvm-options>
                if (System.getProperty("os.name").equalsIgnoreCase("Mac OS X")) {
                    Node newel = doc.createElement("jvm-options");
                    newel.setTextContent("-d32");
                    NodeList nn = doc.getElementsByTagName("java-config");
                    nn.item(0).appendChild(newel);

                }
            }
			port = node.getAttributes().getNamedItem("port"); //$NON-NLS-1$
			port.setNodeValue("" + admin); //$NON-NLS-1$

			Transformer xformer = TransformerFactory.newInstance().newTransformer();
			xformer.transform(new DOMSource(doc), new StreamResult(new File(domainXml)));

		} catch (Exception e) {
			Status status = new Status(IStatus.ERROR, "com.sun.enterprise.jst.server.sunappsrv.configurator", //$NON-NLS-1$
					MessageFormat.format(Messages.ConfigurationOfPortsFailed,e), e);
			Activator.getDefault().getLog().log(status);
			throw new CoreException(status);
		}
	}
}
