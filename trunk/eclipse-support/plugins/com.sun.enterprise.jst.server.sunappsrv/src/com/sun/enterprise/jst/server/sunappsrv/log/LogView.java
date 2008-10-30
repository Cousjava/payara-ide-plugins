// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * glassfishplugins/www/license/CDDLv1.0.txt or
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable,
 * add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your
 * own identifying information: Portions Copyright [yyyy]
 * [name of copyright owner]
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv.log;


import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.TextViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 *
 * author: ludovic champenois
 */
public class LogView extends ViewPart {
	private TableViewer viewer;
	private Action action1;
	private Action action2;
	private Action doubleClickAction;
	private CTabFolder tabFolder ;
	private 		LogThread thread;
	private TreeSet<String> filesViewed;


	/**
	 * The constructor.
	 */
	public LogView() {
		filesViewed = new TreeSet<String>();
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		tabFolder = new CTabFolder(parent, SWT.NONE);

		
	}
	public void init(File file){
		//File file = new File("/Applications/NetBeans/glassfish-v3-prelude-b28/glassfish/domains/domain1/logs/server.log");
		String fn =file.getAbsolutePath();
		if (filesViewed.contains(fn)){
			return;
		}
		filesViewed.add(fn);
		setupLogViewer( file, 500, 1000, true);
		
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				LogView.this.fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(action1);
		manager.add(new Separator());
		manager.add(action2);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(action1);
		manager.add(action2);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(action1);
		manager.add(action2);
	}

	private void makeActions() {
		action1 = new Action() {
			public void run() {
				showMessage("Action 1 executed");
			}
		};
		action1.setText("Action 1");
		action1.setToolTipText("Action 1 tooltip");
		action1.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
			getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		
		action2 = new Action() {
			public void run() {
				showMessage("Action 2 executed");
			}
		};
		action2.setText("Action 2");
		action2.setToolTipText("Action 2 tooltip");
		action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
				getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		doubleClickAction = new Action() {
			public void run() {
				ISelection selection = viewer.getSelection();
				Object obj = ((IStructuredSelection)selection).getFirstElement();
				showMessage("Double-click detected on "+obj.toString());
			}
		};
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}
	private void showMessage(String message) {
		MessageDialog.openInformation(
			viewer.getControl().getShell(),
			"GlassFish Log Viewer",
			message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		tabFolder.setFocus();
	}
	
	

	public void setupLogViewer( File file, int interval, int numLines, boolean saveState)
	{
		// Add the new tab
		CTabItem newItem = new CTabItem(tabFolder, 0);
		newItem.setText(file.getName());

		newItem.setToolTipText(file.getAbsolutePath());
		
		tabFolder.setSelection(newItem);

		TextViewer viewer = new TextViewer(tabFolder, SWT.H_SCROLL | SWT.V_SCROLL);
		newItem.setControl(viewer.getControl());
		final Document newDoc = new Document();
		viewer.setDocument(newDoc);
		viewer.setEditable(false);


		try {
			thread = new LogThread(file, interval, numLines);			
		}
		catch (Exception e) {
			e.printStackTrace();
			return;
		}

		addListener(newDoc, thread, viewer);


		thread.start();

	}
	public void dispose(){
		SunAppSrvPlugin.logMessage("View Log Dispose is CALLED...");
		if (thread!=null){
			thread.halt();
			thread=null;
		}
		super.dispose();
	}
	

	private LogThread.LogListener addListener(final Document doc, LogThread th, final TextViewer viewer){
		final Display display = Display.getCurrent();
		return th.addListener(new LogThread.LogListener()	{
			public void logChanged(final Ring list){
				display.asyncExec(new Runnable(){
					public void run(){
						try{
							StyledText tw = viewer.getTextWidget();
							tw.addLineStyleListener(new LogStyle ());
							if (tw==null){
								SunAppSrvPlugin.logMessage("Error display Log view is null");
								return;
							}

							String b =list.getCompleteDocument();
							tw.append(b);

							// Scroll to the bottom
							viewer.setTopIndex(doc.getNumberOfLines());
						}catch (Exception e){
							SunAppSrvPlugin.logMessage("Error display Log: ",e);

						}
						
					}
				});
			}
		});
	}	

	private static class LogStyle implements LineStyleListener{

		Display display = Display.getCurrent();

		IPreferenceStore store = SunAppSrvPlugin.getInstance().getPreferenceStore();
		boolean colorInConsole= store.getBoolean(PreferenceConstants.ENABLE_COLORS_CONSOLE);

		public void lineGetStyle(LineStyleEvent event) {
			
			String buf = event.lineText;

			// collection for the StyleRanges
			List styles = new ArrayList();

			StyleRange styleRange=null;
			
			if (colorInConsole){
				if (buf.indexOf ("WARNING:")!=-1){
					styleRange = new StyleRange();
					styleRange.start = event.lineOffset;
					styleRange.length = buf.length();
					styleRange.foreground = display.getSystemColor(SWT.COLOR_DARK_YELLOW);
				}else if (buf.indexOf ("SEVERE:")!=-1){
					styleRange = new StyleRange();
					styleRange.start = event.lineOffset;
					styleRange.length = buf.length();
					styleRange.fontStyle = SWT.BOLD;
					styleRange.foreground = display.getSystemColor(SWT.COLOR_RED);
				}else if (buf.indexOf ("INFO:")!=-1){
				}else {
					styleRange = new StyleRange();
					styleRange.start = event.lineOffset;
					styleRange.length = buf.length();
					styleRange.fontStyle = SWT.ITALIC;
				}

				if (styleRange!=null){
					styles.add(styleRange);

					// Set the styles for the line
					event.styles = (StyleRange[]) styles.toArray(new StyleRange[0]);
				}
			}			
		}

	}	
}