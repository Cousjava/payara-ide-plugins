<%@page contentType="text/html"%>
<%@page pageEncoding="MacRoman"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
<head>
 <meta http-equiv="Content-Type" content="text/html; charset=MacRoman" />
<title>Author Detail</title>
</head>
<body>
<f:view>
  <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
 <h1>Author Detail</h1>
<h:form>
  <h:panelGrid columns="2">
<h:outputText value="AuthorId:"/>
 <h:outputText value="#{author.author.authorId}" title="AuthorId" />
<h:outputText value="Organisation:"/>
 <h:outputText value="#{author.author.organisation}" title="Organisation" />
<h:outputText value="Name:"/>
 <h:outputText value="#{author.author.name}" title="Name" />
</h:panelGrid>
<br />
<h:commandLink action="#{author.destroy}" value="Destroy">
<f:param name="jsfcrud.currentAuthor" value="#{author.asString[author.author]}" />
</h:commandLink>
<br />
<br />
<h:commandLink action="#{author.editSetup}" value="Edit">
<f:param name="jsfcrud.currentAuthor" value="#{author.asString[author.author]}" />
</h:commandLink>
<br />
<h:commandLink action="#{author.createSetup}" value="New Author" />
<br />
<h:commandLink action="#{author.listSetup}" value="Show All Author Items"/>
<br />
<a href="/WebAppWithEJBS/faces/welcomeJSF.jsp">Index</a>
</h:form>
 </f:view>
</body>
</html>
