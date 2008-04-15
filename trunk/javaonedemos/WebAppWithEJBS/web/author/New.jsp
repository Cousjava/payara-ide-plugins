<%@page contentType="text/html"%>
<%@page pageEncoding="MacRoman"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
<head>
 <meta http-equiv="Content-Type" content="text/html; charset=MacRoman" />
<title>New Author</title>
</head>
<body>
<f:view>
  <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
 <h1>New Author</h1>
<h:form>
  <h:inputHidden id="validateCreateField" validator="#{author.validateCreate}" value="value"/>
 <h:panelGrid columns="2">
<h:outputText value="AuthorId:"/>
<h:inputText id="authorId" value="#{author.author.authorId}" title="AuthorId" required="true" requiredMessage="The authorId field is required." />
<h:outputText value="Organisation:"/>
<h:inputText id="organisation" value="#{author.author.organisation}" title="Organisation" />
<h:outputText value="Name:"/>
<h:inputText id="name" value="#{author.author.name}" title="Name" />
</h:panelGrid>
<br />
<h:commandLink action="#{author.create}" value="Create"/>
<br />
<br />
<h:commandLink action="#{author.listSetup}" value="Show All Author Items" immediate="true"/>
 <br />
<a href="/WebAppWithEJBS/faces/welcomeJSF.jsp">Index</a>
</h:form>
 </f:view>
</body>
</html>
