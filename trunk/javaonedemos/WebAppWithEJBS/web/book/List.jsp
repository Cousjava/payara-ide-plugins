<%@page contentType="text/html"%>
<%@page pageEncoding="MacRoman"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
<head>
 <meta http-equiv="Content-Type" content="text/html; charset=MacRoman" />
<title>Listing Book Items</title>
</head>
<body>
<f:view>
  <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
 <h1>Listing Book Items</h1>
<h:form>
<h:outputText escape="false" value="(No Book Items Found)<br />" rendered="#{book.itemCount == 0}" />
<h:panelGroup rendered="#{book.itemCount > 0}">
<h:outputText value="Item #{book.firstItem + 1}..#{book.lastItem} of #{book.itemCount}"/>&nbsp;
<h:commandLink action="#{book.prev}" value="Previous #{book.batchSize}" rendered="#{book.firstItem >= book.batchSize}"/>&nbsp;
<h:commandLink action="#{book.next}" value="Next #{book.batchSize}" rendered="#{book.lastItem + book.batchSize <= book.itemCount}"/>&nbsp;
<h:commandLink action="#{book.next}" value="Remaining #{book.itemCount - book.lastItem}"
rendered="#{book.lastItem < book.itemCount && book.lastItem + book.batchSize > book.itemCount}"/>
<h:dataTable value='#{book.books}' var='item' border="0" cellpadding="2" cellspacing="0" rowClasses="jsfcrud_oddrow,jsfcrud_evenrow" rules="all" style="border:solid 1px">
<h:column>
 <f:facet name="header">
 <h:outputText value="BookId"/>
 </f:facet>
 <h:outputText value=" #{item.bookId}"/>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText value="Isbn"/>
 </f:facet>
 <h:outputText value=" #{item.isbn}"/>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText value="Authorid"/>
 </f:facet>
 <h:outputText value=" #{item.authorid}"/>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText value="Publisheddate"/>
 </f:facet>
 <h:outputText value="#{item.publisheddate}">
 <f:convertDateTime type="DATE" pattern="MM/dd/yyyy" />
</h:outputText>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText value="Title"/>
 </f:facet>
 <h:outputText value=" #{item.title}"/>
</h:column>
<h:column>
 <f:facet name="header">
 <h:outputText escape="false" value="&nbsp;"/>
 </f:facet>
<h:commandLink value="Show" action="#{book.detailSetup}">
<f:param name="jsfcrud.currentBook" value="#{book.asString[item]}"/>
</h:commandLink>
  <h:outputText value=" "/>
<h:commandLink value="Edit" action="#{book.editSetup}">
<f:param name="jsfcrud.currentBook" value="#{book.asString[item]}"/>
</h:commandLink>
  <h:outputText value=" "/>
<h:commandLink value="Destroy" action="#{book.destroy}">
<f:param name="jsfcrud.currentBook" value="#{book.asString[item]}"/>
</h:commandLink>
 </h:column>
</h:dataTable>
</h:panelGroup>
<br />
<h:commandLink action="#{book.createSetup}" value="New Book"/>
<br />
<a href="/WebAppWithEJBS/faces/welcomeJSF.jsp">Index</a>

</h:form>
</f:view>
</body>
</html>
