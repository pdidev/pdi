<?xml version='1.0'?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="place_module">
  <xsl:param name="id" select="@id"/>
  <xsl:param name="cname" select="@id"/>
  <xsl:param name="level" select="1"/>
  <xsl:param name="type" select="1"/>
  <xsl:choose>

    <!-- Subgraph de plus haut niveau -->
    <xsl:when test="$level = 1">
      <xsl:variable name="curClust"><xsl:value-of select="substring-before(@id,'/')"/></xsl:variable>
      <xsl:text>subgraph cluster_</xsl:text><xsl:value-of select="$curClust"/><xsl:text> {&#xA;</xsl:text>
      <xsl:text>color=black;&#xA;</xsl:text>
      <xsl:text>label = "</xsl:text><xsl:value-of select="$curClust"/><xsl:text>";&#xA;</xsl:text>
        <xsl:call-template name="place_module">
          <xsl:with-param name="level" select="0"/>
          <xsl:with-param name="id" select="$curClust"/>
          <xsl:with-param name="cname" select="$curClust"/>
          <xsl:with-param name="type" select="$type"/>
        </xsl:call-template>
      <xsl:text>}&#xA;</xsl:text>
    </xsl:when>

    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test="string-length(substring-before(substring-after(@id,concat($id,'/')),'/')) = 0">
        <!-- On est arrivé à la fin, on peut écrire le noeud -->

        <xsl:choose>
        <!-- On choisit le type de noeud -->
        <!-- 1: module, 2: filter, 3: synchronizer, 4: routingnode, 5: routingnodestamp-->
        <xsl:when test="$type = 1">
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
        </xsl:when>

        <xsl:when test="$type = 2">
          <xsl:text>    "</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
          <xsl:text> [id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
          <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
          <xsl:text>label="</xsl:text><xsl:value-of select="@id"/>
          <xsl:if test="@host">
            <xsl:text>\n(</xsl:text><xsl:value-of select="@host"/><xsl:text>)</xsl:text>
          </xsl:if>
            <xsl:text>\n[</xsl:text>
			<xsl:value-of select="filterclass"/>
			<xsl:text>]</xsl:text>
          <xsl:text>" shape=diamond</xsl:text>
          <xsl:if test="@style">
            <xsl:text>,</xsl:text><xsl:value-of select="@style"/>
          </xsl:if>
          <xsl:text>];&#xA;</xsl:text>
        </xsl:when>

        <xsl:when test="$type = 3">
          <xsl:text>    "</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
          <xsl:text>[id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
          <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
          <xsl:text> label="</xsl:text><xsl:value-of select="@id"/>
          <xsl:if test="@host">
            <xsl:text>\n(</xsl:text><xsl:value-of select="@host"/><xsl:text>)</xsl:text>
          </xsl:if>
          <xsl:text>" shape=box</xsl:text>
          <xsl:if test="@style">
            <xsl:text>,</xsl:text><xsl:value-of select="@style"/>
          </xsl:if>
          <xsl:text>];&#xA;</xsl:text>
        </xsl:when>

        <xsl:when test="$type = 4">
          <xsl:text>    "</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
          <xsl:text> [id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
          <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
          <xsl:text>shape=circle,fixedsize=true,width=0.1,height=0.1,fillcolor=black,style=filled,label="</xsl:text>
          <xsl:value-of select="@host"/><xsl:text>\n\n"</xsl:text>
          <xsl:if test="@style">
            <xsl:text>,</xsl:text>
            <xsl:value-of select="@style"/>
          </xsl:if>
          <xsl:text>];&#xA;</xsl:text>
        </xsl:when>

        <xsl:when test="$type = 5">
          <xsl:text>    "</xsl:text><xsl:value-of select="@id"/><xsl:text>"</xsl:text>
          <xsl:text> [id="</xsl:text><xsl:value-of select="@id"/><xsl:text>",</xsl:text>
          <xsl:text>host="</xsl:text><xsl:value-of select="@host"/><xsl:text>",</xsl:text>
          <xsl:text>shape=circle,fixedsize=true,width=0.1,height=0.1,fillcolor=black,style=filled,label="</xsl:text>
          <xsl:value-of select="@host"/><xsl:text>\n\n"</xsl:text>
          <xsl:if test="@style">
            <xsl:text>,</xsl:text><xsl:value-of select="@style"/>
          </xsl:if>
          <xsl:text>];&#xA;</xsl:text>
        </xsl:when>
        </xsl:choose>

        </xsl:when>
        <xsl:otherwise>

          <!-- Il existe encore des niveaux de hierachie, on continue a desccendre -->
          <xsl:variable name="curClust"><xsl:value-of select="concat($id,'/',substring-before(substring-after(@id,concat($id,'/')),'/'))"/></xsl:variable>
          <xsl:variable name="curClustname"><xsl:value-of select="concat($cname,'_',substring-before(substring-after(@id,concat($id,'/')),'/'))"/></xsl:variable>
          <xsl:text>subgraph cluster_</xsl:text><xsl:value-of select="$curClustname"/><xsl:text> {&#xA;</xsl:text>
          <xsl:text>color=black;&#xA;</xsl:text>
         <xsl:text>label = "</xsl:text><xsl:value-of select="$curClust"/><xsl:text>";&#xA;</xsl:text>
            <xsl:call-template name="place_module">
              <xsl:with-param name="level" select="0"/>
              <xsl:with-param name="id" select="$curClust"/>
              <xsl:with-param name="cname" select="$curClustname"/>
              <xsl:with-param name="type" select="$type"/>
            </xsl:call-template>
          <xsl:text>}&#xA;</xsl:text>

        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:param name="Debug" select='0'/>
<xsl:output method="text"/>
<xsl:template match="instanciationgraph|modulelist|network">
<xsl:text>digraph G {&#xA;</xsl:text>
<xsl:text>remincross=true;&#xA;ratio=0.3;&#xA;</xsl:text>
<xsl:text>node [fontname="Helvetica"];&#xA;</xsl:text>




<xsl:for-each select="module">
  <xsl:call-template name="place_module">
     <xsl:with-param name="level" select="1"/>
     <xsl:with-param name="id" select="@id"/>
     <xsl:with-param name="cname" select="@id"/>
     <xsl:with-param name="type" select="1"/>
  </xsl:call-template>
</xsl:for-each>

<xsl:for-each select="filter">
  <xsl:call-template name="place_module">
     <xsl:with-param name="level" select="1"/>
     <xsl:with-param name="id" select="@id"/>
     <xsl:with-param name="cname" select="@id"/>
     <xsl:with-param name="type" select="2"/>
  </xsl:call-template>
</xsl:for-each>

<xsl:for-each select="synchronizer">
  <xsl:call-template name="place_module">
     <xsl:with-param name="level" select="1"/>
     <xsl:with-param name="id" select="@id"/>
     <xsl:with-param name="cname" select="@id"/>
     <xsl:with-param name="type" select="3"/>
  </xsl:call-template>
</xsl:for-each>

<xsl:for-each select="routingnode">
  <xsl:call-template name="place_module">
     <xsl:with-param name="level" select="1"/>
     <xsl:with-param name="id" select="@id"/>
     <xsl:with-param name="cname" select="@id"/>
     <xsl:with-param name="type" select="4"/>
  </xsl:call-template>
</xsl:for-each>

<xsl:for-each select="routingnodestamps">
  <xsl:call-template name="place_module">
     <xsl:with-param name="level" select="1"/>
     <xsl:with-param name="id" select="@id"/>
     <xsl:with-param name="cname" select="@id"/>
     <xsl:with-param name="type" select="5"/>
  </xsl:call-template>
</xsl:for-each>



<!-- Now, we deal with edges -->

<xsl:for-each select="connection">
  <xsl:apply-templates select="source"/>
  <xsl:text> -> </xsl:text>
  <xsl:apply-templates select="destination"/>
  <!-- Add labels to edges just confuse everything -->
  <!--<xsl:text> [labelfloat=true,label="</xsl:text><xsl:value-of select="@id"/><xsl:text>" ]</xsl:text>-->
  <xsl:text> [</xsl:text>
  <xsl:if test="@style">
    <xsl:text>,</xsl:text><xsl:value-of select="@style"/>
  </xsl:if>
  <xsl:text>];&#xA;</xsl:text>
</xsl:for-each>

<xsl:for-each select="connectionstamps">
  <xsl:apply-templates select="sourcestamps"/>
  <xsl:text> -> </xsl:text>
  <xsl:apply-templates select="destinationstamps"/>
  <!-- Add labels to edges just confuse everything -->
  <!--<xsl:text> [labelfloat=true,label="</xsl:text><xsl:value-of select="@id"/><xsl:text>" ]</xsl:text>-->
  <xsl:text> [style=dashed,weight=0.5</xsl:text>
  <xsl:if test="@style">
    <xsl:text>,</xsl:text><xsl:value-of select="@style"/>
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
