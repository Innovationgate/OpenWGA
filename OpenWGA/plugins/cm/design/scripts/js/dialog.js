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
/**
 * JS finctions to handle modal dialogs
 */

BI.dialog={};


BI.showCustomDialog = function(tml){
	BI.dialog.show("custom-dialog", null, {tml:tml});
}

/**
 * object to store all dialogs in
 */
BI.dialog.dialogs={};

/**
 * create a dialog
 * valid config options:
 * 		id:	the name of the tml to include as portlet
 * 		title: the title of the dialog
 * 		submitButtonText: (optional) alternative text for submit button
 * 		closeButtonText: (optional) alternative text for close button
 * 		submitOnEnter (boolean): 		
 * @param {String} name
 * @param {Object} config
 */
BI.dialog.create=function(name, config){
	BI.dialog.dialogs[name]=config;
}

BI.dialog.hide=function(el){
	BI.dialog.dialog.animateTarget=null;
	BI.dialog.dialog.hide();	
}
BI.dialog.onShow=function(){}		// should be overwritten in dialog portlets
BI.dialog.onHide=function(){}		// should be overwritten in dialog portlets
BI.dialog.onBeforeShow=function(){}	// should be overwritten in dialog portlets


/**
 * calculate the size of the dialog and resize it
 */
BI.dialog.autoHeight=function(){
	
	BI.makeInputFields("BI-dialog");
	BI.makeComboBoxes("BI-dialog");
	
	var dialog_el=BI.dialog.dialog.getEl();
	var body_el=BI.dialog.dialog.body;
	body_el.setStyle("overflow", "hidden");
	var inner_el=dialog_el.child(".BI-dialog");
	inner_el.setWidth(body_el.getWidth(true));
	var hdif=body_el.getHeight()- inner_el.getHeight();
	BI.dialog.dialog.resizeTo(500, 5+dialog_el.getHeight()-hdif);
}
			
/**
 * refresh the dialog: add css and create Ext.fields from input fields
 */
BI.dialog.refresh=function(){
	//BI.dialog.autoHeight();
}

/**
 * initialised the given dialog using the stored config options
 * @param {String} dialogname: id of the dialog
 * @param {Object} el:	the element to start animation from
 */
BI.dialog.init=function(dialogname, el){
	BI.dialog.el=el;

	var dialog = BI.dialog.config = BI.dialog.dialogs[dialogname];
	if(!dialog){
		alert("Dialog " + dialogname + " ist not defined.")
		return false;
	}
	
	BI.dialog.submitOnEnter = dialog.submitOnEnter;
	
	var dialogobj=BI.dialog.dialog;
	dialogobj.setTitle(dialog.title);
	if(dialog.submitButtonText){
		BI.dialog.submitButton.show();
		BI.dialog.submitButton.setText(dialog.submitButtonText);
		BI.dialog.closeButton.setText($L.cancel);
	}
	else {
		BI.dialog.submitButton.hide();	
		BI.dialog.closeButton.setText($L.close);
	}

	if(dialog.closeButtonText){
		BI.dialog.closeButton.setText(dialog.closeButtonText);
	}
	BI.dialog.closeButton.show();
	
	var pos=dialogobj.getEl().getCenterXY(true);
	dialogobj.moveTo(pos[0], 34);
	
	return true;
}

/**
 * utility function to change the title of the dialog after initialisation
 * @param {Object} title
 */
BI.dialog.setTitle=function(title){
	BI.dialog.dialog.setTitle(title);
}

/**
 * create one instance of Ext.BasicDialog used for all dialogs
 */
BI.dialog.initModalDialog=function(){
	Ext.useShims=true;

	var dialog = BI.dialog.dialog = new Ext.BasicDialog("BI-dialog", { 
        modal:true,
		resizable: false,
		collapsible: false,
		shim: true,
		constraintoviewport: true,
        width:500,
        height:300
	});	
	dialog.addKeyListener(27, BI.dialog.hide, dialog);
	dialog.addKeyListener(13, function(a,b,ev){
		if(BI.dialog.submitOnEnter){
			BI.dialog.submit()
			ev.stopEvent();		// don't submit any form
		}
	}, dialog);
	BI.dialog.submitButton = dialog.addButton($L.save, function(){BI.dialog.submit()}, dialog);
	BI.dialog.closeButton = dialog.addButton($L.cancel, BI.dialog.hide, dialog);
	dialog.on("beforeShow", function() {
		BI.dialog.onBeforeShow();
	});
	dialog.on("show", function(){
		BI.dialog.onShow();
	});
	dialog.on("hide", function(){
		BI.dialog.onHide();
	});
	
};

/****/

for(d in $L.dialogs){
	var params=$L.dialogs[d];
	params.tml="cms:dialogs:"+params.id;
	BI.dialog.create(params.id, params);
}

Ext.get("init").update("Dialogs loaded ...")
