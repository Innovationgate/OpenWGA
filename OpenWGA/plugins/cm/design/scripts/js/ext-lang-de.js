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
/*
 * Ext JS Library 1.0 Beta 1
 * Copyright(c) 2006-2007, Ext JS, LLC.
 * licensing@extjs.com
 * 
 * http://www.extjs.com/license
 */

/*
Ext.View.prototype.emptyText = "";
Ext.grid.Grid.prototype.ddText = "%0 Zeile(n) ausgewält";
Ext.TabPanelItem.prototype.closeText = "Diesen Tab schließen";
Ext.form.Field.prototype.invalidText = "Der Wert des Feldes ist nicht korrekt";
Ext.UpdateManager.defaults.indicatorText = '<div class="loading-indicator">übertrage Daten ...</div>';
*/

Ext.UpdateManager.defaults.indicatorText = '<div class="loading-indicator">übertrage Daten ...</div>';

if(Ext.View){
   Ext.View.prototype.emptyText = "";
}

if(Ext.grid.Grid){
   Ext.grid.Grid.prototype.ddText = "{0} Zeile(n) ausgewählt";
}

if(Ext.TabPanelItem){
   Ext.TabPanelItem.prototype.closeText = "Diesen Tab schließen";
}

if(Ext.form.Field){
   Ext.form.Field.prototype.invalidText = "Der Wert des Feldes ist nicht korrekt";
}

Date.monthNames = [
   "Januar",
   "Februar",
   "März",
   "April",
   "Mai",
   "Juni",
   "Juli",
   "August",
   "September",
   "Oktober",
   "November",
   "Dezember"
];

Date.dayNames = [
   "Sonntag",
   "Montag",
   "Dienstag",
   "Mittwoch",
   "Donnerstag",
   "Freitag",
   "Samstag"
];

Ext.MessageBox.buttonText = {
   ok     : "OK",
   cancel : "Abbrechen",
   yes    : "Ja",
   no     : "Nein"
};

Ext.util.Format.date = function(v, format){
   if(!v) return "";
   if(!(v instanceof Date)) v = new Date(Date.parse(v));
   return v.dateFormat(format || "d.m.Y");
};

Ext.apply(Ext.DatePicker.prototype, {
   todayText         : "Heute",
   minText           : "Dieses Datum liegt von dem erstmöglichen Datum",
   maxText           : "Dieses Datum liegt nach dem letztmöglichen Datum",
   disabledDaysText  : "",
   disabledDatesText : "",
	monthNames		: Date.monthNames,
	dayNames		: Date.dayNames,
   nextText          : 'Nächster Monat (Strg/Control + Rechts)',
   prevText          : 'Vorheriger Monat (Strg/Control + Links)',
   monthYearText     : 'Monat auswählen (Strg/Control + Hoch/Runter, um ein Jahr auszuwählen)',
   todayTip          : "Heute ({0}) (Leertaste)",
   format            : "d.m.Y"
});

Ext.apply(Ext.PagingToolbar.prototype, {
   beforePageText : "Seite",
   afterPageText  : "von {0}",
   firstText      : "Erste Seite",
   prevText       : "vorherige Seite",
   nextText       : "nächste Siete",
   lastText       : "letzte Seite",
   refreshText    : "Aktualisieren",
   displayMsg     : "Anzeige Eintrag {0} - {1} von {2}",
   emptyMsg       : 'Keine Daten vorhanden'
});

Ext.apply(Ext.form.TextField.prototype, {
   minLengthText : "Bitte geben Sie mindestens {0} Zeichen ein",
   maxLengthText : "Bitte geben Sie maximal {0} Zeichen ein",
   blankText     : "Dieses Feld darf nich leer sein",
   regexText     : "",
   emptyText     : null
});

Ext.apply(Ext.form.NumberField.prototype, {
   minText : "Der Mindestwert für dieses Feld ist {0}",
   maxText : "Der Maximalwert für dieses Feld ist {0}",
   nanText : "{0} ist keine Zahl"
});

Ext.apply(Ext.form.DateField.prototype, {
   disabledDaysText  : "nicht erlaubt",
   disabledDatesText : "nicht erlaubt",
   minText           : "Das Datum in diesem Feld muß nach dem {0} liegen",
   maxText           : "Das Datum in diesem Feld muß vor dem {0} liegen",
   invalidText       : "{0} ist kein valides Datum - es muß im Format {1} eingegeben werden",
   format            : "Tag.Monat.Jahr"
});

Ext.apply(Ext.form.ComboBox.prototype, {
   loadingText       : "Lade Daten ...",
   valueNotFoundText : undefined
});

Ext.apply(Ext.form.VTypes, {
   emailText    : 'Dieses Feld sollte eine E-Mail-Adresse enthalten. Format: "user@domain.com"',
   urlText      : 'Dieses Feld sollte eine URL enthalten. Format "http:/'+'/www.domain.com"',
   alphaText    : 'Dieses Feld darf zur Buchstaben enthalten und _',
   alphanumText : 'Dieses Feld darf zur Buchstaben und Zahlen enthalten und _'
});

Ext.apply(Ext.grid.GridView.prototype, {
   sortAscText  : "Aufsteigend sortieren",
   sortDescText : "Absteigend sortieren",
   lockText     : "Spalte sperren",
   unlockText   : "Spalte freigeben (entsperren)",
   columnsText  : "Spalten"
});

Ext.apply(Ext.grid.PropertyColumnModel.prototype, {
   nameText   : "Name",
   valueText  : "Wert",
   dateFormat : "d.m.Y"
});

Ext.apply(Ext.SplitLayoutRegion.prototype, {
   splitTip            : "Ziehen, um Größe zu ändern.",
   collapsibleSplitTip : "Ziehen, um Größe zu ändern. Doppelklick um Panel auszublenden."
});
