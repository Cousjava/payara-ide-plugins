<%@page contentType="text/html"%>
<%@page pageEncoding="MacRoman"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
<head>
 <meta http-equiv="Content-Type" content="text/html; charset=MacRoman" />
<title>Listing Author Items</title>
</head>
<body>
<f:view>
  <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
 <h1>Listing Author Items</h1>
<h:form>
<h:outputText escape="false" value="(No Author Items Found)<br />" rendered="#{author.itemCount == 0}" />
<h:panelGroup rendered="#{author.itemCount > 0}">
<h:outputText value="Item #{author.firstItem + 1}..#{author.lastItem} of #{author.itemCount}"/>&nbsp;
<h:commandLink action="#{author.prev}" value="Previous #{author.batchSize}" rendered="#{author.firstItem >= author.batchSize}"/>&nbsp;
<h:commandLink action="#{author.next}" value="Next #{author.batchSize}" rendered="#{author.lastItem + author.batchSize <= author.itemCount}"/>&nbsp;
<h:commandLink action="#{author.next}" value="Remaining #{author.itemCount - author.lastItem}"
rendered="#{author.lastItem < author.itemCount && author.lastItem + author.batchSize > author.itemCount}"/>
<h:dataTable value='#{author.authors}' var='item' border="0" cellpadding="2" cellspacing="0" rowClasses="jsfcrud_oddrow,jsfcrud_evenrow" rules="all" style="border:solid 1px">
<h:column>
 <f:facet name="header">
 <h:outputText value="AuthorId"/>
 </f:facet>
 <h:outputText value=" #{item.authorId}"/>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText value="Organisation"/>
 </f:facet>
 <h:outputText value=" #{item.organisation}"/>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText value="Name"/>
 </f:facet>
 <h:outputText value=" #{item.name}"/>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText escape="false" value="&nbsp;"/>
 </f:facet>
<h:commandLink value="Show" action="#{author.detailSetup}">
<f:param name="jsfcrud.currentAuthor" value="#{author.asString[item]}"/>
</h:commandLink>
  <h:outputText value=" "/>
<h:commandLink value="Edit" action="#{author.editSetup}">
<f:param name="jsfcrud.currentAuthor" value="#{author.asString[item]}"/>
</h:commandLink>
  <h:outputText value=" "/>
<h:commandLink value="Destroy" action="#{author.destroy}">
<f:param name="jsfcrud.currentAuthor" value="#{author.asString[item]}"/>
</h:commandLink>
 </h:column>
</h:dataTable>
</h:panelGroup>
<br />
<h:commandLink action="#{author.createSetup}" value="New Author"/>
<br />
<a href="/WebAppWithEJBS/faces/welcomeJSF.jsp">Index</a>

</h:form>
</f:view>
</body>
</html>
