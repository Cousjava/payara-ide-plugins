package org.sun.importWizards;



import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map.Entry;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;


import org.sun.Activator;
import org.sun.gf.nbecl.converter.Converter;
import org.sun.gf.nbecl.factory.DotProjectFactory;



public class ImportProjectWizard extends Wizard implements IImportWizard {
	private ImportPageWizard mainPage;
	
	public ImportProjectWizard() {
		// TODO Auto-generated constructor stub
		super();
	}

	public boolean performFinish() {
		if (mainPage.canFinish()) {
			HashMap<String, String> vals = new HashMap<String, String>();
			
			//String path = FileLocations.getResourcesDirectory();
			try {
				
				String projectName = DotProjectFactory.getProjectName(mainPage.path.toString());
				vals.put("$projectname$", projectName);
				vals.put("$userhome$", System.getProperty("user.home"));
				IProject project = createBaseProject(projectName, mainPage.path);
				File dotsettings =new File(mainPage.path.toString() +"/.settings");
				if(!dotsettings.isDirectory())
					dotsettings.mkdirs();
				HashMap<String, String> files = new HashMap<String, String>();
				
				files.put(".settings/org.eclipse.jdt.core.prefs", "org.eclipse.jdt.core.prefs");
				files.put(".settings/org.eclipse.jst.j2ee.ejb.annotations.xdoclet.prefs", "org.eclipse.jst.j2ee.ejb.annotations.xdoclet.prefs");
				files.put(".settings/org.eclipse.jst.jsf.designtime.appmgr.prefs", "org.eclipse.jst.jsf.designtime.appmgr.prefs");
				files.put(".settings/org.eclipse.wst.common.component", "org.eclipse.wst.common.component");
				files.put(".settings/org.eclipse.wst.common.project.facet.core.prefs.xml", "org.eclipse.wst.common.project.facet.core.prefs.xml");
				files.put(".settings/org.eclipse.wst.common.project.facet.core.xml", "org.eclipse.wst.common.project.facet.core.xml");
				files.put(".settings/org.eclipse.wst.jsdt.ui.superType.container", "org.eclipse.wst.jsdt.ui.superType.container");
				files.put(".settings/org.eclipse.wst.jsdt.ui.superType.name", "org.eclipse.wst.jsdt.ui.superType.name");
				files.put(".settings/.jsdtscope", ".jsdtscope");
				files.put(".classpath", ".classpath");
				files.put(".project", ".project");
				boolean success = new File("/.settings").mkdir();
				for (Entry<String, String> e : files.entrySet())
					createTemplateFile(project, e.getKey(), e.getValue(), vals);
				org.sun.gf.nbecl.converter.Converter.convert(mainPage.path.toString());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			return true;
		}
		return false;
	}
	
	private static void createTemplateFile(IProject project, String filepath, String filesource, HashMap<String, String> vals) throws CoreException {
		IFile template = project.getFile(new Path(filepath));
		InputStream in = openTemplateContentStream(project, filesource, vals);
		if(template.exists())
			template.setContents(in, true, true, null);
		else
			try{
				template.create(in, true, null);
			}catch ( Exception e )
			{
				IFolder folder = project.getFolder("/.settings");
				folder.create(true, true, null);
				template.create(in, true, null);
			}
		JavaCore.create(template);
	}
	
	private static InputStream openTemplateContentStream(IProject project, String filename, HashMap<String, String> vals) {
		//http://eclipse-javacc.cvs.sourceforge.net/viewvc/eclipse-javacc/sf.eclipse.javacc/src-plugin/sf/eclipse/javacc/wizards/JJNewWizard.java?view=markup
		//eclipse plugin distributing template files
		final URL installURL = Activator.getDefault().getBundle().getResource("utils/");
		URL url;
		try {
			url = new URL(installURL, filename);
			return makeTemplateInputStream(url.openStream(), vals);
		} catch (final MalformedURLException e) {
			e.printStackTrace();
		} catch (final IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		setNeedsProgressMonitor(true);
	}
	
	public void addPages() {
		super.addPages();
		mainPage = new ImportPageWizard("Import Glassfish Project");
		addPage(mainPage);
	}
	
	public boolean canFinish(){
		return mainPage.canFinish();
	}
	
	private static IProject createBaseProject(String projectName, IPath location) {
		IProject newProject = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		
		if (!newProject.exists()) {
			IPath projectLocation = location;
			IProjectDescription desc = newProject.getWorkspace().newProjectDescription(newProject.getName());
			if (location != null &&
					ResourcesPlugin.getWorkspace().getRoot().getLocation().equals(location)) {
				projectLocation = null;
			}
			desc.setLocation(projectLocation);
			try {
				newProject.create(desc, null);
				if (!newProject.isOpen()) {
					newProject.open(null);
				}
			} catch (CoreException e) {
	            e.printStackTrace();
			}
		}
		
		JavaCore.create(newProject);
		return newProject;
	}
	
	public static String readInput(InputStream stream) {
		StringBuffer buffer = new StringBuffer();
		try {
			InputStreamReader isr = new InputStreamReader(stream);
			Reader in = new BufferedReader(isr);
			int ch;
			while ((ch = in.read()) > -1) {
				buffer.append((char)ch);
				System.out.print("'"+(char) ch+"'");
			}
			System.out.print("\n");
			in.close();
			return buffer.toString();
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private static InputStream makeTemplateInputStream(InputStream stream, HashMap<String, String> vals) {
		String str;
		try {
			str = readInput(stream);
			stream.close();
		} catch (final IOException e) {
			e.printStackTrace();
			return null;
		}
		
		// Instantiate template
		for (Entry<String, String> e : vals.entrySet())
			str = str.replace(e.getKey(), e.getValue());

		return new ByteArrayInputStream(str.getBytes());
	}

}
