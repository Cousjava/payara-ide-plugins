package com.sun.enterprise.jst.server.sunappsrv.configurator.test;

import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.eclipse.core.runtime.Platform;
import org.eclipse.wst.server.core.IRuntime;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.sun.enterprise.jst.server.sunappsrv.configurator.V2Configurator;
import com.sun.enterprise.jst.server.sunappsrv.configurator.V3Configurator;

/**
 * Before running this test from workspace, ensure that the target platform
 * contains glassfish V3 prelude, or system property "gf3location" is pointing
 * to Prelude.
 */
public class PluginTest extends TestCase {

	private static final String TMP_DEST_DIR = "/tmp/destDir";
	private static final String DOMAIN_XML = TMP_DEST_DIR
			+ "/domain1/config/domain.xml";

	public void setUp() throws Exception {
		System.out.println(Platform.getWS());
		System.out.println(Platform.getLocation());
	}

	public void tearDown() throws Exception {
		File file = new File(TMP_DEST_DIR);
		if (file.exists()) {
			deleteFolder(file);
		}
	}

	/**
	 * Recursive delete files.
	 */
	void deleteFolder(File dir) {
		String[] ls = dir.list();

		for (int idx = 0; idx < ls.length; idx++) {
			File file = new File(dir, ls[idx]);
			if (file.isDirectory()) {
				deleteFolder(file);
			}
			file.delete();
		}
		dir.delete();
	}

	public void testCopyDomains() throws Exception {
		File file = new File(TMP_DEST_DIR);
		assertFalse("Destination file exists!", file.exists());

		V3Configurator.copyDomain(TMP_DEST_DIR);
		assertTrue("Destination file doesn't exist!", file.exists());

		assertTrue("domain1.xml file doesn't exist", new File(DOMAIN_XML)
				.exists());
	}

	public void testConfigureDomain() throws Exception {
		V3Configurator.copyDomain(TMP_DEST_DIR);
		File domainXml = new File(DOMAIN_XML);
		assertTrue("domain1.xml file doesn't exist", domainXml.exists());

		DocumentBuilderFactory domFactory = DocumentBuilderFactory
				.newInstance();
		domFactory.setNamespaceAware(true);
		DocumentBuilder builder = domFactory.newDocumentBuilder();
		Document doc = builder.parse(domainXml);

		XPathFactory factory = XPathFactory.newInstance();
		XPath xpath = factory.newXPath();
		XPathExpression expr = xpath
				.compile("//http-listener[@id='http-listener-1']");
		Node node = (Node) expr.evaluate(doc, XPathConstants.NODE);
		Node port = node.getAttributes().getNamedItem("port");
		assertEquals("8080", port.getNodeValue());

		expr = xpath.compile("//http-listener[@id='admin-listener']");
		node = (Node) expr.evaluate(doc, XPathConstants.NODE);
		port = node.getAttributes().getNamedItem("port");
		assertEquals("4848", port.getNodeValue());

		V3Configurator.setPortsForDomain(DOMAIN_XML, 18080, 14848);

		// Test new values
		doc = builder.parse(domainXml);

		expr = xpath.compile("//http-listener[@id='http-listener-1']");
		node = (Node) expr.evaluate(doc, XPathConstants.NODE);
		port = node.getAttributes().getNamedItem("port");
		assertEquals("18080", port.getNodeValue());

		expr = xpath.compile("//http-listener[@id='admin-listener']");
		node = (Node) expr.evaluate(doc, XPathConstants.NODE);
		port = node.getAttributes().getNamedItem("port");
		assertEquals("14848", port.getNodeValue());

	}
	
	public void testCreateRuntimes() throws Exception {
		IRuntime v2 = V2Configurator.createRuntime(V3Configurator.getGlassfishLocation());
		IRuntime v3 = V3Configurator.createRuntime(V3Configurator.getGlassfishLocation());
		assertNotNull(v2);
		assertNotNull(v3);

		assertEquals("GlassFish v2.1 Java EE 5",v2.getId());
		assertEquals("GlassFish v3 Prelude",v3.getId());
	}
}
