<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/">
<html>
  <style>
  .idtooltip {
      position: relative;
  }

  .idtooltiptext:after {
      content: attr(tooltiptext);
  }

  .idtooltip .idtooltiptext {
      visibility: hidden;
      background-color: black;
      color: #fff;
      text-align: center;
      border-radius: 6px;
      padding: 5px 20px;
      opacity: 0.8;

      /* Position the tooltip */
      position: absolute;
      z-index: 1;
  }

  .idtooltip:hover .idtooltiptext {
      visibility: visible;
  }

  .paragraph {
      margin: 10px 110px;
      text-align: justify;
  }

  .table {
      margin: auto;
  }

  .page_start {
      font-weight:bold;
      background-color: #96C3DC;
      color: #fff;
      padding: 2px 4px;
  }

  .page_start:after {
      content: attr(page_number);
      font-weight:bold;
      background-color: #96C3DC;
      color: #fff;
  }

  .gloss {
      font-weight:bold; 
      color: #2C9221;
  }

  .vernacular {
      font-style:italic;
      font-weight:bold;
      color: #1B63A5;
  }

  .bg_blue1 {
      background-color: #1B63A5;
      color: #fff;
  }

  .bg_blue2 {
      background-color: #96C3DC;
      color: #fff;
  }

  .bg_green1 {
      background-color: #2C9221;
      color: #fff;
  }

  .bg_green2 {
      background-color: #A4DB78;
      color: #fff;
  }

  .bg_red1 {
      background-color: #D90017;
      color: #fff;
  }

  .bg_red2 {
      background-color: #F48486;
      color: #fff;
  }

  .interlinear_example{
      background-color: #eee;
      border-radius: 6px;
      padding: 10px;
      margin: 5px 0;     
  }

  .example_number:after {
      content: attr(x_number);
      padding-right: 10px;
      vertical-align: text-top;
      color: #444;
  }
  
    .example_subnumber:after {
      content: attr(x_sub_number);
      padding-right: 10px;
      vertical-align: text-top;
      color: #444;
  }

  .pre_spaced:before {
      content: " ";
  }
  </style>
  
  <body style="font-family: 'Georgia', Times; font-size:18; line-height: 140%">
    <h1>Grammar Layout Markup View</h1> 
    <xsl:apply-templates/>  
  </body>

</html>


</xsl:template>

<!--Mute everything in other nodes-->
<xsl:template match="//Abbyy_html" />
<!--xsl:template match="//layout_xml" /-->

<xsl:template match="para">
  <p html_id="{@html_id}" id="{@id}">
    <div class="paragraph">
      <span>
        <xsl:apply-templates />
      </span>
      <span> </span>
      <span class="idtooltip bg_blue2">@
        <span class="idtooltiptext" tooltiptext="{@html_id} #{@id}"/>
      </span>
    </div>
  </p>
</xsl:template>

<xsl:template match="sentence">
  <span>
    <xsl:apply-templates />
  </span>    
  <span class="idtooltip bg_green2">@<span class="idtooltiptext" tooltiptext="#{@id}"/></span>  
</xsl:template>

<xsl:template match="table">
  <p align="center" html_id="{@html_id}" id="{@id}">
    <span class="idtooltip bg_green2">@<span class="idtooltiptext" tooltiptext="{@html_id} #{@id}"/></span>
  </p>
  <table class="table" border="1">
  <xsl:apply-templates />
  </table>
  <br/>
</xsl:template>

<xsl:template match="tr">
  <tr html_id="{@html_id}" id="{@id}">
    <xsl:apply-templates />
    <td>
      <span class="idtooltip bg_blue2">@<span class="idtooltiptext" tooltiptext="{@html_id} #{@id}"/></span>
    </td>
  </tr>
</xsl:template>

<xsl:template match="td">
  <xsl:copy>
    <xsl:copy-of select="@id"/>
    <xsl:copy-of select="@colspan"/>
    <xsl:apply-templates />
  </xsl:copy>
</xsl:template>

<xsl:template match="i">
  <span style="font-style:italic;">
  <xsl:apply-templates />
  </span>
</xsl:template>

<xsl:template match="u">
  <span style="text-decoration:underline;">
  <xsl:apply-templates />
  </span>
</xsl:template>

<xsl:template match="ui">
  <span style="text-decoration:underline;font-style:italic;">
  <xsl:apply-templates />
  </span>
</xsl:template>

<xsl:template match="page_start">
  <span class="page_start" page_number="{@page_number}">page </span>
</xsl:template>

<xsl:template match="abbr">
  <span class="gloss">
  <xsl:apply-templates />
  </span>
</xsl:template>

<xsl:template match="v">
  <span class="vernacular">
  <xsl:apply-templates />
  </span>
</xsl:template>

<xsl:template match="//line[@xpart='xf' and not(@x_last)]">
  <span>
  <xsl:apply-templates />
  </span><br/><br/>
</xsl:template>

<xsl:template match="x">
  <div class="interlinear_example">
    <div style="display: inline-block; vertical-align: top; width: 80px;">
      <span class="idtooltip example_number" x_number="{@x_number}">
        <span class="idtooltiptext" tooltiptext="{@html_id} #{@id}"/>
      </span>
      <span class="example_subnumber" x_sub_number="{@x_sub_number}"/>
    </div>
    <div style="display: inline-block; border-left: 1px solid #888;">
      <table border="0">
        <tr>
        <xsl:apply-templates select="xh"/>
        <xsl:apply-templates select="xfg"/>
        </tr>
      </table>   
    </div>
  </div>
</xsl:template>

<xsl:template match="xh">
  <td style="font-weight:bold; padding-left: 10px;">
  <xsl:apply-templates />
  </td>
</xsl:template>

<xsl:template match="xfg">
  <xsl:apply-templates select="xgg"/>
  <xsl:apply-templates select="xf"/>    
</xsl:template>

<xsl:template match="xgg">
  <table border="0">
  <xsl:apply-templates />
  </table>
</xsl:template>

<xsl:template match="xu">
  <tr>
  <xsl:apply-templates />
  </tr>
</xsl:template>

<xsl:template match="xv">
  <tr>
  <xsl:apply-templates />
  </tr>
</xsl:template>

<xsl:template match="xg">
  <tr>
  <xsl:apply-templates />
  </tr>
</xsl:template>

<xsl:template match="intx">
  <span style="background-color: #ddd; border-radius: 4px;">
    <xsl:apply-templates select="v"/>
    <span class="pre_spaced">
      <xsl:apply-templates select="intxg"/> 
    </span>
  </span>
</xsl:template>

<xsl:template match="xw">
  <td style="padding-left: 10px; padding-right: 20px;">
  <xsl:apply-templates />
  </td>
</xsl:template>

<xsl:template match="xf">
  <span style="padding-left: 10px;">
  <xsl:apply-templates />
  </span>
</xsl:template>

<xsl:template match="img">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
    </xsl:copy>
</xsl:template>

</xsl:stylesheet>