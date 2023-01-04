/*******************************************************************************
 *Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 *
 *This file is part of the OpenWGA server platform.
 *
 *OpenWGA is free software: you can redistribute it and/or modify
 *it under the terms of the GNU General Public License as published by
 *the Free Software Foundation, either version 3 of the License, or
 *(at your option) any later version.
 *
 *In addition, a special exception is granted by the copyright holders
 *of OpenWGA called "OpenWGA plugin exception". You should have received
 *a copy of this exception along with OpenWGA in file COPYING.
 *If not, see <http://www.openwga.com/gpl-plugin-exception>.
 *
 *OpenWGA is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with OpenWGA in file COPYING.
 *If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
// rtftoolbar_txt_de.js

AFW.RTFToolbar.txt={
	tooltips:{
		save:				"Speichern",
		paste:				"Text ohne Formatierung aus der Zwischenablage einfügen",
		bold: 				"Fett",
		italic:				"Kursiv",
		underline:			"Unterstrichen",
		forecolor:			"Textfarbe",
		removeformat:		"Alle Formatierungen entfernen",
		indent:				"Text einrücken",
		outdent:			"Text ausrücken",
		justifyleft:		"Text links ausrichten",
		justifycenter:		"Text zentrieren",
		justifyright:		"Text rechts ausrichten",
		insertunorderedlist:"Bulletliste",
		insertorderedlist:	"Nummerierte Liste",
		inserttable:		"Tabelle einfügen",
		tableproperties:	"Tabelleneigenschaften bearbeiten",
		inserttablerow:		"Zeile in Tabelle einfügen",
		deletetablerow:		"Tabellenzeile löschen",
		inserttablecol:		"Spalte in Tabelle einfügen",
		deletetablecol:		"Spalte löschen",
		mergetablecells:	"Spalten rechts zusammenführen",
		splittablecell:		"Spalten rechts spliten",
		insertlink:			"Link in Dialog auswählen",
		unlink:				"Link entfernen",
		insertsimplelink:	"Link einfügen oder bearbeiten",
		insertimg:			"Grafik aus Bibliothek einfügen",
		insertsimpleimg:	"Grafik einfügen"
	},

	linkDialog:{
		title:				"Link einfügen",
		linktitle:			"Titel",
		url:				"Adresse:",
		target:				"Zielfenster:",
		linktype:			"Linkart:",
		linktypeValues:		["extern|exturl", "Dateianhang|intfile"]
	},

	imgDialog:{
		title:				"Grafik einfügen",
		url:				"Adresse:",
		alt:				"Titel:",
		border:				"Rahmen:",
		align:				"Ausrichtung:",
		alignValues:		["unten|bottom", "mitte|middle", "oben|top", "links|left", "rechts|right"]
	},
	
	fileDialog:{
		title:				"Anhänge verwalten",
		files:				"Dataianhänge"
	},
	
	tableDialog:{
		title:				"Tabelle einfügen",
		width:				"Breite:",
		align:				"Ausrichtung:",
		alignValues:		["keine|", "links|left", "zentriert|center", "rechts|right"],
		rows:				"Zeilen:",
		cols:				"Spalten:"
	},

	tablePropDialog:{
		title:				"Tabellenattribute ändern",
		change:				"Ändern",
		to:					"in",
		applyto:			"anwenden auf",
		preview:			"Vorschau",
		none:				["kein Stil|"],
		tableStyleList:		["Tablenstil", "Zeilenstil", "Zellenstil"],
		applyToTableList:	["ausgewählte Table|table"],
		applyToTrList:		["ausgewählte Zeile|selected", "alle Zeilen der Tabelle|all"],
		applyToTdList:		["ausgewählte Zelle|selected", "alle Zeilen der Tabelle|row", "alle Zellen der Tabelle|all"]
	},
	
	viewmodes_html:			["WYSIWYG|wysiwyg", "HTML|html", "Vorschau|preview"],
	viewmodes:				["WYSIWYG|wysiwyg", "Vorschau|preview"]
	
};
