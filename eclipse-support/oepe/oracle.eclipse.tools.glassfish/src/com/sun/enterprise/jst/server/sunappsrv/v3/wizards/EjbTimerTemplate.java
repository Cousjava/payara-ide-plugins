package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import java.util.*;
import org.eclipse.jst.j2ee.internal.common.operations.*;

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

@SuppressWarnings("restriction")
public class EjbTimerTemplate {
  protected static String nl;
  public static synchronized EjbTimerTemplate create(String lineSeparator)
  {
    nl = lineSeparator;
    EjbTimerTemplate result = new EjbTimerTemplate();
    nl = null;
    return result;
  }

  public final String NL = nl == null ? (System.getProperties().getProperty("line.separator")) : nl;
  protected final String TEXT_1 = "package ";
  protected final String TEXT_2 = ";";
  protected final String TEXT_3 = NL;
  protected final String TEXT_4 = NL + "import ";
  protected final String TEXT_5 = ";";
  protected final String TEXT_6 = NL + NL + "@Stateless";
  protected final String TEXT_7 = NL + "public ";
  protected final String TEXT_8 = "abstract ";
  protected final String TEXT_9 = "final ";
  protected final String TEXT_10 = "class ";
  protected final String TEXT_11 = " extends ";
  protected final String TEXT_12 = " implements ";
  protected final String TEXT_13 = ", ";
  protected final String TEXT_14 = " {";
  protected final String TEXT_15 = NL + NL + "    /**" + NL + "     * Default constructor. " + NL + "     */" + NL + "    public ";
  protected final String TEXT_16 = "() {" + NL + "        // TODO Auto-generated constructor stub" + NL + "    }";
  protected final String TEXT_17 = NL + "       " + NL + "    /**" + NL + "     * @see ";
  protected final String TEXT_18 = "#";
  protected final String TEXT_19 = "(";
  protected final String TEXT_20 = ")" + NL + "     */" + NL + "    public ";
  protected final String TEXT_21 = "(";
  protected final String TEXT_22 = ") {" + NL + "        super(";
  protected final String TEXT_23 = ");" + NL + "        // TODO Auto-generated constructor stub" + NL + "    }";
  protected final String TEXT_24 = NL + NL + "\t/**" + NL + "     * @see ";
  protected final String TEXT_25 = "#";
  protected final String TEXT_26 = "(";
  protected final String TEXT_27 = ")" + NL + "     */" + NL + "    public ";
  protected final String TEXT_28 = " ";
  protected final String TEXT_29 = "(";
  protected final String TEXT_30 = ") {" + NL + "        // TODO Auto-generated method stub";
  protected final String TEXT_31 = NL + "\t\t\treturn ";
  protected final String TEXT_32 = ";";
  protected final String TEXT_33 = NL + "    }";
  protected final String TEXT_34 = NL + "\t";
  protected final String TEXT_35 = NL + "\t@SuppressWarnings(\"unused\")" + NL + "\t@Schedule(";
  protected final String TEXT_36 = ")" + NL + "    private void scheduledTimeout(final Timer t) {" + NL + "        System.out.println(\"@Schedule called at: \" + new java.util.Date());" + NL + "    }" + NL + "}";

	public String generate(Object argument)
  {
    final StringBuffer stringBuffer = new StringBuffer();
     AddEjbTimerTemplateModel model = (AddEjbTimerTemplateModel) argument; 
     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	model.removeFlags(CreateJavaEEArtifactTemplateModel.FLAG_QUALIFIED_SUPERCLASS_NAME); 

     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	if (model.getJavaPackageName() != null && model.getJavaPackageName().length() > 0) {

    stringBuffer.append(TEXT_1);
    stringBuffer.append( model.getJavaPackageName() );
    stringBuffer.append(TEXT_2);
    
	}

    stringBuffer.append(TEXT_3);
     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
     
	Collection<String> imports = model.getImports();
	for (String anImport : imports) { 

    stringBuffer.append(TEXT_4);
    stringBuffer.append( anImport );
    stringBuffer.append(TEXT_5);
     
	}

    stringBuffer.append(TEXT_6);
     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	if (model.isPublic()) { 

    stringBuffer.append(TEXT_7);
     
	} 

	if (model.isAbstract()) { 

    stringBuffer.append(TEXT_8);
    
	}

	if (model.isFinal()) {

    stringBuffer.append(TEXT_9);
    
	}

    stringBuffer.append(TEXT_10);
    stringBuffer.append( model.getClassName() );
    
	String superClass = model.getSuperclassName();
 	if (superClass != null && superClass.length() > 0) {

    stringBuffer.append(TEXT_11);
    stringBuffer.append( superClass );
    
	}

	List<String> interfaces = model.getInterfaces(); 
 	if ( interfaces.size() > 0) { 

    stringBuffer.append(TEXT_12);
    
	}
	
 	for (int i = 0; i < interfaces.size(); i++) {
   		String INTERFACE = (String) interfaces.get(i);
   		if (i > 0) {

    stringBuffer.append(TEXT_13);
    
		}

    stringBuffer.append( INTERFACE );
    
	}

    stringBuffer.append(TEXT_14);
     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
     
	if (!model.hasEmptySuperclassConstructor()) { 

    stringBuffer.append(TEXT_15);
    stringBuffer.append( model.getClassName() );
    stringBuffer.append(TEXT_16);
     
	} 

	if (model.shouldGenSuperclassConstructors()) {
		List<Constructor> constructors = model.getConstructors();
		for (Constructor constructor : constructors) {
			if (constructor.isPublic() || constructor.isProtected()) { 

    stringBuffer.append(TEXT_17);
    stringBuffer.append( model.getSuperclassName() );
    stringBuffer.append(TEXT_18);
    stringBuffer.append( model.getSuperclassName() );
    stringBuffer.append(TEXT_19);
    stringBuffer.append( constructor.getParamsForJavadoc() );
    stringBuffer.append(TEXT_20);
    stringBuffer.append( model.getClassName() );
    stringBuffer.append(TEXT_21);
    stringBuffer.append( constructor.getParamsForDeclaration() );
    stringBuffer.append(TEXT_22);
    stringBuffer.append( constructor.getParamsForCall() );
    stringBuffer.append(TEXT_23);
    
			} 
		} 
	} 

     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	if (model.shouldImplementAbstractMethods()) {
		for (Method method : model.getUnimplementedMethods()) { 

    stringBuffer.append(TEXT_24);
    stringBuffer.append( method.getContainingJavaClass() );
    stringBuffer.append(TEXT_25);
    stringBuffer.append( method.getName() );
    stringBuffer.append(TEXT_26);
    stringBuffer.append( method.getParamsForJavadoc() );
    stringBuffer.append(TEXT_27);
    stringBuffer.append( method.getReturnType() );
    stringBuffer.append(TEXT_28);
    stringBuffer.append( method.getName() );
    stringBuffer.append(TEXT_29);
    stringBuffer.append( method.getParamsForDeclaration() );
    stringBuffer.append(TEXT_30);
     
			String defaultReturnValue = method.getDefaultReturnValue();
			if (defaultReturnValue != null) { 

    stringBuffer.append(TEXT_31);
    stringBuffer.append( defaultReturnValue );
    stringBuffer.append(TEXT_32);
    
			} 

    stringBuffer.append(TEXT_33);
     
		}
	} 

    stringBuffer.append(TEXT_34);
    
		String schedule = model.getProperty(AddEjbTimerDataModelProvider.SCHEDULE).trim();
	
    stringBuffer.append(TEXT_35);
    stringBuffer.append( schedule );
    stringBuffer.append(TEXT_36);
    return stringBuffer.toString();
  }
}
