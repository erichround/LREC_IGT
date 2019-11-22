<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>

<xsl:template match="/">
<html>
  <style>
  .idtooltip {
      position: relative;
      background-color: #1B63A5;
      color: #fff;
  }

  .idtooltiptext:after {
      content: attr(tooltiptext);
  }

  .idtooltip .idtooltiptext {
      visibility: hidden;
      width: 120px;
      background-color: black;
      color: #fff;
      text-align: center;
      border-radius: 6px;
      padding: 5px 0;
      opacity: 0.8;

      /* Position the tooltip */
      position: absolute;
      z-index: 1;
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

  .idtooltip:hover .idtooltiptext {
      visibility: visible;
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
  <p html_id="{@html_id}">
    <span class="idtooltip bg_blue2">@
      <span class="idtooltiptext" tooltiptext="{@html_id}"/>
    </span>
    <xsl:apply-templates />
  </p>
</xsl:template>

<xsl:template match="table">
  <table border="1" html_id="{@html_id}">
  <xsl:apply-templates />
  </table>
</xsl:template>

<xsl:template match="tr">
  <tr html_id="{@html_id}">
    <td>
      <span class="idtooltip bg_blue2">@
        <span class="idtooltiptext" tooltiptext="{@html_id}"/>
      </span>
    </td>
    <xsl:apply-templates />
  </tr>
</xsl:template>

<xsl:template match="td">
  <td>
  <xsl:apply-templates />
  </td>
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
  <span class="page_start" page_number="{@page_number}">page </span>
</xsl:template>

<xsl:template match="line">
  <xsl:apply-templates />
  <br/>
</xsl:template>

<xsl:template match="x">
  <span class="idtooltip bg_green1">@
    <span class="idtooltiptext" tooltiptext="Id: {@html_id}"/>
  </span>
  <xsl:apply-templates />
</xsl:template>

</xsl:stylesheet>