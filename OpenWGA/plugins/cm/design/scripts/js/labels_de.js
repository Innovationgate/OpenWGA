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
$L={
	cancel: "Abbrechen",
	close: "Schlie�en",
	save: "Speichern",
	saveandclose: "Speichern & Schlie�en",
	edit: "Bearbeiten",
	edit_field: "Feld bearbeiten",
	cancel_edit: "Bearbeitung abbrechen",
	please_wait: "Bitte warten",
	filename: "Dateiname",
	filesize: "Gr��e",
	remove: "L�schen",
	deleting: "L�sche ..."
}

$L.headings=[
	"�berschrift 1",
	"�berschrift 2",
	"�berschrift 3",
	"�berschrift 4",
	"�berschrift 5",
	"�berschrift 6"
]

$L.goto={
	title: "Gehe zu ...",
	msg: "Bitte geben Sie einen Schl�ssel oder eindeutigen Namen ein"
}

$L.layout={
	sitesstructure: "Sitestruktur"
}

$L.statusbar={
	contentdocument: "Inhaltsdokument",
	loaded: "geladen",
	no_content_for_page: "Kein Inhaltsdokument f�r Seite",
	found: "gefunden",
	no_wga_page: "Angezeigte Seite ist keine OpenWGA Inhaltsseite.",
	loading_page: "lade Seite"
}

$L.editors= {
	click_to_edit: "Klicken, um dieses Seitenelement zu bearbeiten"
}

$L.searchpanel= {
	searchresult: "Suchergebnis",
	button_text : "Neue Suche ...",
	click_here_to_search: "<h1>Neue Suche</h1>Klicken Sie hier, um eine neue Suche<br> mit neuen Parametern zu starten"
}

$L.propertypannel = {
	title: "Inhaltseigenschaften",
	button_tooltip: "<h1>Speichern</h1>Klicken Sie hier, um Ihre �nderungen zu speichern" 
}

$L.contentviewpanel = {
	title: "Inhalte"
}

$L.se={
	tooltip_reload_se : "Siteexplorer aktualisieren",
	tooltip_copy: "Seite in die Zwischenablage kopieren",
	tooltip_paste: "Seite aus der Zwischenablage einf�gen",
	tooltip_create_rootpage: "Neue Hauptseite",
	tooltip_create_childpage: "Neue Unterseite",
	tooltip_delete_page: "Seite l�schen",
	tooltip_page_settings: "Seiteneigenschaften"
}

$L.dialogs=[
	{
		id: 				"login",
		title: 				"Benutzer oder Berechtigung wechseln ...",
		submitButtonText: 	"Anmelden",
		submitOnEnter: 		true		
	},
	{
		id: 				"admin-login",
		title: 				"Als OpenWGA Administrator anmelden ...",
		submitButtonText: 	"Anmelden",
		submitOnEnter: 		true		
	},
	{
		id: 				"linkchecker-config",
		title: 				"Linkchecker Konfiguration ...",
		submitButtonText: 	"Konfiguration speichern",
		submitOnEnter: 		false		
	},
	{
		id: 				"quit",
		title: 				"Content Manager beenden ...",
		submitButtonText: 	"Beenden",
		submitOnEnter: 		true
	},
	{
		id: "publish-content",
		title: 				"Inhalt ver�ffentlichen ...",
		submitButtonText:	"Ver�ffentlichen",
		submitOnEnter:		false
	},

	{
		id: "create-page",
		title: 				"Neue Seite erstellen ...",
		submitButtonText:	"Seite erstellen",
		submitOnEnter:		true
	},
	
	{
		id: "create-content",
		title: 				"Neue Sprachversion erstellen ...",
		submitButtonText:	"Sprachversion erstellen",
		submitOnEnter:		true
	},
	
	{	id: "delete-page",
		title: 				"Seite und alle Unterseiten l�schen ...",
		submitButtonText:	"Seite l�schen",
		submitOnEnter:		true
	},
	
	{	id: "delete-content",
		title: 				"Inhaltsversion l�schen ...",
		submitButtonText:	"Version l�schen",
		submitOnEnter:		true
	},
	
	{	id: "archive-content",
		title: 				"Inhaltsversion archivieren ...",
		submitButtonText:	"Version archivieren",
		submitOnEnter:		true
	},
	
	{	id: "show-access",
		title: 				"Berechtigungen ..."
	},
		
	{	id: "approve-content",
		title: 				"Inhalt genehmigen ...",
		submitButtonText:	"Genehmigen"
	},
	
	{	id: "reject-content",
		title: 				"Freigabe des Inhalts ablehnen ...",
		submitButtonText:	"Ablehnen"
	},

	{	id: "reject-pending-content",
		title: 				"Freigabestatus zur�cknehmen ...",
		submitButtonText:	"In Arbeit setzen"
	},
	
	{	id: "edit-content",
		title: 				"Inhalt bearbeiten ...",
		submitButtonText:	"Arbeitskopie erzeugen",
		submitOnEnter:		true
	},
	
	{	id: "settings",
		title: 				"Einstellungen bearbeiten ...",
		submitButtonText:	"Speichern",
		submitOnEnter:		true
	},
	
	{	id: "search", 
		title: 				"Inhaltsdokument suchen ...",
		submitButtonText:	"Suchen",
		submitOnEnter:		true
	},
	
	{	id: "move-page", 
		title: 				"Seite verscheiben ...",
		submitButtonText:	"Verschieben",
		submitOnEnter:		true
	},
	
	{	id: "clear-remote", 
		title: 				"Verbindung zu Dokument l�schen ...",
		submitButtonText:	"Verbindung l�schen",
		submitOnEnter:		true
	},

	{	id: "about",
		title: 				"�ber OpenWGA Content Manager ..."
	},

	{	id: "upload",
		title: 				"Datei hochladen ...",
		submitButtonText:	"Hochladen",
		closeButtonText:	"Schlie�en"
	},

	{	id: "forward-content",
		title: 				"Inhalt weiterleiten ...",
		submitButtonText:	"Mail senden"
	},

	{	id: "userdefined-metas",
		title: 				"Benutzerdefinierte Einstellungen ...",
		submitButtonText:	"Speichern",
		no_combo_boxes:		true
	},

	{	id: "workflow-level",
		title: 				"Workflow Level ...",
		submitButtonText:	"Speichern"
	},

	{	id: "paste-page",
		title: 				"Seite aus der Zwischenablage einf�gen ...",
		submitButtonText:	"Einf�gen"
	},

	{	id: "paste-content",
		title: 				"Inhaltselemente aus der Zwischenablage einf�gen ...",
		submitButtonText:	"Einf�gen"
	},

	{	id: "ldap-lookup",
		title: 				"LDAP Verzeichnis ..."
	},
	
	{
		id: "rtf:insert-link",
		title: 				"Link einf�gen ...",
		submitButtonText: 	"Einf�gen",
		submitOnEnter:		true
	},

	{
		id: "rtf:insert-image",
		title: 				"Grafik einf�gen ...",
		submitButtonText: 	"Einf�gen",
		submitOnEnter:		true
	},
	
	{
		id: "bookmarks:add-bookmark",
		title: 				"Bookmark hinzuf�gen ...",
		submitButtonText: 	"Speichern",
		submitOnEnter: 		true
	},
	
	{
		id: "bookmarks:manage-bookmarks",
		title: "Bookmarks verwalten ...",
		submitOnEnter: true
	},	

	{
		id: "check-links",
		title: "Links pr�fen ...",
		submitButtonText: 	"Links pr�fen"
	}	

	,{
		id: "start-agent",
		title: "Starte Agent ...",
		submitButtonText: 	"Ausf�hren"
	}
	
	,{
		id: "tag-editor",
		title: "Schl�sselw�rter",
		submitButtonText: "Speichern"
	}

	,{
		id: "init-db-design",
		title: "Datenbank initialisierung",
		submitButtonText: "Designelemente erzeugen"
	}

	,{
		id: "rtf:edit-rtf-node",
		title: "RTF Element bearbeiten",
		submitButtonText: "Aktualisieren"
	}

	,{
		id: "scale-image",
		title: "Bild Skalieren",
		submitButtonText: "Bild skalieren"
	}
		
	,{
		id: "view-image",
		title: "Bild betrachten",
		cancelButtonText: "Schliessen"
	}
	
	,{
		id: "userdefined-section",
		title: "Abschnittsinhalt",
		submitButtonText: "Einf�gen"
	}

	,{
		id: "content-modules",
		title: "Inhaltsmodule",
		submitButtonText: "Speichern"
	}

	,{
		id: "empty-trash",
		title: "Papierkorb",
		submitButtonText: "Leeren"
	}

	,{
		id: "restore-deleted-page",
		title: "Gel�schte Seite wiederherstellen",
		submitButtonText: "Wiederherstellen"
	}
	
	,{
		id: "user-info",
		title: "User Information"
	}

	,{
		id: "page-settings",
		title: "Seiteneigenschaften",
		submitButtonText: "Speichern"
	}

	,{
		id: "image-item-editor",
		title: "Bild ausw�hlen",
		submitButtonText: "Speichern"
	}

	,{
		id: "custom-dialog",
		title: "Custom Dialog",
		submitButtonText: "Submit"
	}

	,{
		id: "file-meta-data",
		title: "Datei Metadaten",
		submitButtonText: "Aktualisieren"
	}
	
	,{
		id: "edit-video",
		title: "Video",
		submitButtonText: "Poster aktualisieren"
	}

];


/*
 * designer labels (ses)
 */

$L.toolbarmenu = {
	newContentType: "Neuer Seitentyp",
	newTmlModule: "Neues TML Modul",
	newArea: "Neuer Bereich",
	newLanguage: "Neue Sprache",
	newCssJs: "Neues Script/CSS Modul",
	newFileContainer: "Neuer Dateicontainer",
	newWorkflow: "Neuer Workflow"
}

$L.designExplorer = {
	designTabTitle: "Verwaltung",
	refreshTooltip: "Aktualisieren"
}

$L.panels = {
	tml: "TML",
	contenttype: "Seitentyp",
	area: "Bereich",
	language: "Sprache",
	cssjs: "Script/CSS",
	filecontainer: "Dateicontainer",
	workflow: "Workflow",
	
	alternativeParam: "(neu)"
}

$L.contentComparePanel={
	title: "Inhalte vergleichen"
}


$L.RTFToolbar={
	zoom: "Feld Zoomen",
	editHelper: "Absatzmarkierungen ein-/ausblenden",
	undo: "R�ckg�ngig",
	redo: "Wiederholen",
	paste: "Ohne Formattierungen einf�gen",
	removeFormatting: "Alle Formatierungen entfernen",
	bold: "Fett",
	italic: "Kursiv",
	underline: "Unterstrichen",
	justifyLeft: "Linksb�ndig",
	justifyRight: "Rechtsb�ndig",
	justifyCenter: "Zentriert",
	justifyFull: "Blocksatz",
	indent: "Einr�cken",
	outdent: "Ausr�cken",
	bulletList: "Unnummerierte Liste",
	enumList: "Nummerierte Liste",
	insertLink: "Link erstellen/�ndern",
	insertTable: "Tabelle einf�gen",
	insertImage: "Bild erstellen/�ndern",
	editImage: "Bild bearbeiten",
	unlink: "Link l�schen",
	save_and_continue: "Zwischenspeichern",
	save_and_close: "Speichern und Schlie�en" 
}

$L.linktype={
	exturl: "Externer Link",
	int: "Interner Link",
	intname: "Interner Link per eindeutigem Namen",
	intfile: "Datei aus diesem Dokument",
	extfile: "Datei aus beliebigem Dokument",
	file: "Datei aus Dateicontainer",
	primaryfile: "Prim�re Datai aus Dokument"		
}
