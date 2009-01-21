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
import org.eclipse.core.runtime.FileLocator;
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

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class V3Configurator {


	public static void configure(IProgressMonitor progressMonitor)
			throws CoreException {
		progressMonitor.setTaskName("Creating Glassfish V3 prelude server configuration.");
		String glassfishLocation = getGlassfishLocation();
		try {
			IServerType st = ServerCore.findServerType(Constants.SERVER_PRELUDE_ID);// v3
			IRuntime runtime = createRuntime(glassfishLocation);
			IServer[] servers = ServerCore.getServers();

			for (IServer server : servers) {
				if (server.getRuntime() == null) {
					server.delete();
				}
				if (runtime != null && server != null && runtime.equals(server.getRuntime())) {
					return;
				}
			}

			IServerWorkingCopy wc = st.createServer(null, null, runtime, null);
			wc.setName("Bundled " + runtime.getName());

			SunAppServer sunAppServer = (SunAppServer) wc.getAdapter(SunAppServer.class);

			String domainLocation = Platform.getLocation().append(".metadata").append(".plugins").append(
					Constants.SERVER_PRELUDE_ID).toOSString();
			copyDomain(domainLocation);
			setPortsForDomain(new Path(domainLocation).append("domain1").append("config").append("domain.xml")
					.toOSString(), 18080, 14848);
			Map<String, String> configuration = sunAppServer.getProps();
			configuration.put(SunAppServer.DOMAINDIR, domainLocation);
			sunAppServer.setServerInstanceProperties(configuration);

			wc.save(true, null);

			// startServer(sunAppServer);
		} catch (CoreException e) {
			e.printStackTrace();
		}
		
	}

	private static String getGlassfishLocation() {
		String property = System.getProperty("gf3location");
		String glassfishLocation = null;
		if (property != null) {
			glassfishLocation = property;

		} else {
			try {
				// Get the eclipse installation location and from it V2
				// installation directory.
				glassfishLocation = new Path(
						Platform.getInstallLocation().getURL().getFile()).toPortableString()
						+ "/glassfishv3/glassfish";
						    
				SunAppSrvPlugin.logMessage("glassfishV3Loc =" + glassfishLocation);
				return glassfishLocation;
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		}
		return glassfishLocation;
	}

	@SuppressWarnings("unchecked")
	public static IRuntime createRuntime(String glassfishLocation) {
		try {
			IServerType st = ServerCore.findServerType(Constants.SERVER_PRELUDE_ID);
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
			e.printStackTrace();
		}
		return null;
	}

	public static void copyDomain(String toDir) throws CoreException {
		String srcDir = getDomainsDir();
		File destDir = new File(toDir);
		if (destDir.exists()) {
			throw new CoreException(new Status(IStatus.ERROR, "com.sun.enterprise.jst.server.sunappsrv.configurator",
					"Domain destination directory exists"));
		}
		destDir.mkdirs();
		copyDir(new File(srcDir), destDir);

	}

	private static void copyDir(File srcDir, File destDir) throws CoreException {
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
						throw new CoreException(new Status(IStatus.ERROR,
								"com.sun.enterprise.jst.server.sunappsrv.configurator", "File copying failed due to: "
										+ e));
					}
				}
			}
		}
	}

	public static String getDomainsDir() throws CoreException {
		return new Path(getGlassfishLocation()).append("domains").toOSString();
	}

	public static void setPortsForDomain(String domainXml, int i, int j) throws CoreException {
		try {
			DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
			domFactory.setNamespaceAware(true);
			DocumentBuilder builder = domFactory.newDocumentBuilder();
			Document doc = builder.parse(domainXml);

			XPathFactory factory = XPathFactory.newInstance();
			XPath xpath = factory.newXPath();
			XPathExpression expr = xpath.compile("//http-listener[@id='http-listener-1']");
			Node node = (Node) expr.evaluate(doc, XPathConstants.NODE);
			Node port = node.getAttributes().getNamedItem("port");
			port.setNodeValue("" + i);

			expr = xpath.compile("//http-listener[@id='admin-listener']");
			node = (Node) expr.evaluate(doc, XPathConstants.NODE);
			port = node.getAttributes().getNamedItem("port");
			port.setNodeValue("" + j);

			Transformer xformer = TransformerFactory.newInstance().newTransformer();
			xformer.transform(new DOMSource(doc), new StreamResult(new File(domainXml)));

		} catch (Exception e) {
			throw new CoreException(new Status(IStatus.ERROR, "com.sun.enterprise.jst.server.sunappsrv.configurator",
					"Configuration of ports failed because of: " + e));
		}
	}
}
