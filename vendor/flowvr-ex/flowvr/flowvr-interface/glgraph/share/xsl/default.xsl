<?xml version='1.0'?>
<!-- Definitions -->
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:param name="Debug" select='0'/>
<xsl:output method="text"/>

<!-- Replace string function -->
<xsl:template name="replace-string">
    <xsl:param name="text"/>
    <xsl:param name="replace"/>
    <xsl:param name="with"/>
    <xsl:choose>
      <xsl:when test="contains($text,$replace)">
        <xsl:value-of select="substring-before($text,$replace)"/>
        <xsl:value-of select="$with"/>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text"
select="substring-after($text,$replace)"/>
          <xsl:with-param name="replace" select="$replace"/>
          <xsl:with-param name="with" select="$with"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>



<!-- Root template -->
<xsl:template match="network">
  
  <xsl:text>digraph G {&#xA;</xsl:text>
  <xsl:text>node [fontname="Helvetica"];&#xA;</xsl:text>
  <!-- <xsl:text>[ratio=0.3]</xsl:text> -->
  
  
  <xsl:apply-templates select="composite|module|filter|synchronizer|routingnode|routingnodestamps" />
    
  <!-- Connections -->
  <xsl:for-each select="connection">
    <xsl:apply-templates select="source"/>
    <xsl:text> -> </xsl:text>
    <xsl:apply-templates select="destination"/>
    <xsl:text> [label="</xsl:text><xsl:value-of select="@id"/><xsl:text>" ]</xsl:text>
    <xsl:if test="@style">
        <xsl:text>[</xsl:text>
        <xsl:value-of select="@style"/>
        <xsl:text>]</xsl:text>
    </xsl:if>
    <xsl:text>;&#xA;</xsl:text>
  </xsl:for-each>
  
  <!-- Connections stamp -->
  <xsl:for-each select="connectionstamps">
    <xsl:apply-templates select="sourcestamps"/>
    <xsl:text> -> </xsl:text>
    <xsl:apply-templates select="destinationstamps"/>
    <xsl:text> [label="</xsl:text><xsl:value-of select="@id"/><xsl:text>" ]</xsl:text>
    <xsl:text> [style=dashed</xsl:text>
    <xsl:if test="@style">
        <xsl:text>,</xsl:text>
        <xsl:value-of select="@style"/>
    </xsl:if>
    <xsl:text>];&#xA;</xsl:text>
  </xsl:for-each>
    
    
  <xsl:text>}&#xA;</xsl:text>
  
</xsl:template> <!-- End network -->


<!-- Composite template -->
<xsl:template match="composite">
  <xsl:text>subgraph cluster_</xsl:text>
  
    <xsl:call-template name="replace-string">
      <xsl:with-param name="text" select="@id"/>
      <xsl:with-param name="replace" select="'-'" />
      <xsl:with-param name="with" select="'_'"/>
    </xsl:call-template>
  <!-- <xsl:value-of select="@id"/> -->
  
  
  <xsl:text> {&#xA;</xsl:text>

  <xsl:apply-templates select="composite|module|filter|synchronizer|routingnode|routingnodestamps" />
  
  <xsl:text>}&#xA;</xsl:text>
</xsl:template>


<!-- Module template -->
<xsl:template match="module">
    <xsl:text>"</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
    <xsl:text> [id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
    <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
    <xsl:text>label="{</xsl:text>
    
    <xsl:for-each select="input">
        <xsl:text>{</xsl:text>
        <xsl:for-each select="port">
            <xsl:if test="position()&gt;1"><xsl:text>|</xsl:text></xsl:if>
            <xsl:text>&lt;</xsl:text><xsl:value-of select="@id"/><xsl:text>&gt;</xsl:text>
            <xsl:value-of select="@id"/>
            
	          <xsl:if test="@blockstate">
		           <xsl:text>: </xsl:text>
          		 <xsl:value-of select="@blockstate"/>
	          </xsl:if>
         </xsl:for-each>
         <xsl:text>}|</xsl:text>
    </xsl:for-each>
    
    <xsl:text>{</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:if test="@host">
      <xsl:text>\n(</xsl:text>
      <xsl:value-of select="@host"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
    <xsl:text>}</xsl:text>
    
    <xsl:for-each select="output">
      <xsl:text>|{</xsl:text>
      <xsl:for-each select="port">
        <xsl:if test="position()&gt;1"><xsl:text>|</xsl:text></xsl:if>
        <xsl:text>&lt;</xsl:text><xsl:value-of select="@id"/><xsl:text>&gt;</xsl:text>
        <xsl:value-of select="@id"/>
      </xsl:for-each>
      <xsl:text>}</xsl:text>
    </xsl:for-each>
    
    <xsl:text>}",shape=Mrecord,fontname="Helvetica",style=bold</xsl:text>
    <xsl:if test="@style">
      <xsl:text>,</xsl:text>
      <xsl:value-of select="@style"/>
    </xsl:if>
    
    <xsl:text>];&#xA;</xsl:text>
    
    <xsl:apply-templates select="composite|module|filter|synchronizer|routingnode|routingnodestamps" />
    
</xsl:template> <!-- End module -->

<!-- Filters -->
<xsl:template match="filter">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
  <xsl:text> [id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
  <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
  <xsl:text>label="</xsl:text>
  <xsl:value-of select="@id"/>
  <xsl:if test="@host">
    <xsl:text>\n(</xsl:text>
    <xsl:value-of select="@host"/>
    <xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:text>\n[</xsl:text>
  <xsl:value-of select="filterclass"/>
  <xsl:text>]</xsl:text>
  <xsl:text>" shape=diamond</xsl:text>
  <xsl:if test="@style">
    <xsl:text>,</xsl:text>
    <xsl:value-of select="@style"/>
  </xsl:if>
  <xsl:text>];&#xA;</xsl:text>
  
  <xsl:apply-templates select="composite|module|filter|synchronizer|routingnode|routingnodestamps" />
</xsl:template>

<!-- Synchronizers -->
<xsl:template match="synchronizer">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
  <xsl:text>[id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
  <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
  <xsl:text> label="</xsl:text>
  <xsl:value-of select="@id"/>
  <xsl:if test="@host">
      <xsl:text>\n(</xsl:text>
      <xsl:value-of select="@host"/>
      <xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:text>" shape=box</xsl:text>
  <xsl:if test="@style">
    <xsl:text>,</xsl:text>
    <xsl:value-of select="@style"/>
  </xsl:if>
  <xsl:text>];&#xA;</xsl:text>
  
  <xsl:apply-templates select="composite|module|filter|synchronizer|routingnode|routingnodestamps" />
</xsl:template>

<!-- Routing nodes -->
<xsl:template match="routingnode">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
  <xsl:text> [id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
  <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
  <xsl:text>shape=circle,fixedsize=true,width=0.1,height=0.1,fillcolor=black,style=filled,label="</xsl:text>
  <xsl:value-of select="@host"/>
  <xsl:text>\n\n"</xsl:text>
  <xsl:if test="@style">
    <xsl:text>,</xsl:text>
    <xsl:value-of select="@style"/>
  </xsl:if>
  <xsl:text>];&#xA;</xsl:text>
  
  <xsl:apply-templates select="composite|module|filter|synchronizer|routingnode|routingnodestamps" />
</xsl:template>

<!-- Routing nodes stamp -->
<xsl:template match="routingnodestamps">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
  <xsl:text> [id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
  <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
  <xsl:text>shape=circle,fixedsize=true,width=0.1,height=0.1,fillcolor=black,style=filled,label="</xsl:text>
  <xsl:value-of select="@host"/>
  <xsl:text>\n\n"</xsl:text>
  <xsl:if test="@style">
    <xsl:text>,</xsl:text>
    <xsl:value-of select="@style"/>
  </xsl:if>
  <xsl:text>];&#xA;</xsl:text>
  
  <xsl:apply-templates select="composite|module|filter|synchronizer|routingnode|routingnodestamps" />
</xsl:template>
    



<xsl:template match="source">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="destination">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="sourcestamps">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="destinationstamps">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="moduleid">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/>
  <xsl:if test="@port">
    <xsl:text>":"</xsl:text><xsl:value-of select="@port"/>
  </xsl:if>
  <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="routingnodeid">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="routingnodestampsid">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="filterid">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/>
<!--
  <xsl:if test="@port">
    <xsl:text>":"</xsl:text><xsl:value-of select="@port"/>
  </xsl:if>
-->
  <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="synchronizerid">
  <xsl:text>"</xsl:text><xsl:value-of select="@id"/>
<!--
  <xsl:if test="@port">
    <xsl:text>":"</xsl:text><xsl:value-of select="@port"/>
  </xsl:if>
-->
  <xsl:text>"</xsl:text>
</xsl:template>
</xsl:transform>
