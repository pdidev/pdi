<?xml version='1.0'?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:param name="Debug" select='0'/>
<xsl:output method="text"/>
<xsl:template match="instanciationgraph|modulelist|network">
<xsl:text>digraph G {&#xA;</xsl:text>
<xsl:text>node [fontname="Helvetica"];&#xA;</xsl:text>

   
<xsl:for-each select="module[not(@host = preceding-sibling::module/@host)]">

   <!-- Beginning og the sub-graph -->
   <xsl:text>subgraph cluster_</xsl:text><xsl:value-of select="@host"/><xsl:text> {&#xA;</xsl:text>
   <xsl:variable name="curHost"><xsl:value-of select="@host"/></xsl:variable>


   <xsl:for-each select="../module[@host=$curHost]">
      <xsl:text>    </xsl:text>
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
			<xsl:if test="@blockstate">
			<xsl:text>: </xsl:text>
			<xsl:value-of select="@blockstate"/>
			</xsl:if>
        </xsl:for-each>
        <xsl:text>}</xsl:text>
      </xsl:for-each>
      <xsl:text>}",shape=Mrecord,fontname="Helvetica",style=bold</xsl:text>
      <xsl:if test="@style">
        <xsl:text>,</xsl:text>
        <xsl:value-of select="@style"/>
      </xsl:if>
      <xsl:text>];&#xA;</xsl:text>
    </xsl:for-each>



    <xsl:for-each select="../filter[@host=$curHost]">
      <xsl:text>    </xsl:text>
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
      <xsl:text>" shape=diamond</xsl:text>
      <xsl:if test="@style">
        <xsl:text>,</xsl:text>
        <xsl:value-of select="@style"/>
      </xsl:if>
      <xsl:text>];&#xA;</xsl:text>
    </xsl:for-each>




    <xsl:for-each select="../synchronizer[@host=$curHost]">
      <xsl:text>    </xsl:text>
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
    </xsl:for-each>




    <xsl:for-each select="../routingnode[@host=$curHost]">
      <xsl:text>    </xsl:text>
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
    </xsl:for-each>




    <xsl:for-each select="../routingnodestamps[@host=$curHost]">
      <xsl:text>    </xsl:text>
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
    </xsl:for-each>



   <xsl:text>}&#xA;</xsl:text>
   <!-- End of the sub-graph -->


</xsl:for-each>















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
    <xsl:for-each select="connectionstamps">
      <xsl:apply-templates select="sourcestamps"/>
      <xsl:text> -> </xsl:text>
      <xsl:apply-templates select="destinationstamps"/>
      <xsl:text> [label="</xsl:text><xsl:value-of select="@id"/><xsl:text>" ]</xsl:text>
        <xsl:text> [style=dashed,weight=0.5</xsl:text>
	<xsl:if test="@style">
          <xsl:text>,</xsl:text>
          <xsl:value-of select="@style"/>
        </xsl:if>
      <xsl:text>];&#xA;</xsl:text>
    </xsl:for-each>
  <xsl:text>}&#xA;</xsl:text>
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
