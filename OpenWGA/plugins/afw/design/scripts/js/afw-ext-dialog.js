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
 * JS functions to handle modal dialogs
 */

AFW.dialog={};

AFW.dialog.callAction=function(action, message){
	AFW.callAction(action, AFW.dialog.portletkey, message)
}

/**
 * object to store all dialogs in
 */
AFW.dialog.dialogs={};

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
AFW.dialog.create=function(name, config){
	AFW.dialog.dialogs[name]=config;
}

AFW.dialog.hide=function(el){
	if(AFW.dialog.dialog){
		//AFW.dialog.dialog.animateTarget=null;
		AFW.dialog.dialog.hide();
	}
	//else console.log("called AFW.dialog.hide with no dialog", AFW.dialog)	
}
AFW.dialog.onShow=function(){}		// should be overwritten in dialog portlets

AFW.dialog.focus=function(field){
	var form = document.forms[AFW.dialog.config.tml+"-form"];
	try{
		form.elements[field].focus();
	}
	catch(e){
		//console.log("field '" + field + "' not found.", form.elements)
	}
}

AFW.dialog.getField=function(field){
	var form = document.forms[AFW.dialog.config.tml+"-form"];
	return form.elements[field];
}

/**
 * calculate the size of the dialog and resize it
 */
AFW.dialog.autoHeight=function(){
	
	/*
	AFW.makeInputFields("AFW-dialog");
	AFW.makeComboBoxes("AFW-dialog");
	*/

	var dialog_el=AFW.dialog.dialog.getEl();
	var body_el=AFW.dialog.dialog.body;
	var inner_el=dialog_el.child(".dialog");
	inner_el.setWidth(body_el.getWidth(true));
	var hdif=body_el.getHeight()- inner_el.getHeight();
	var h = 5+dialog_el.getHeight()-hdif;
	if(h<130)
		h=130;
	AFW.dialog.dialog.resizeTo(500, h);
}
			
/**
 * refresh the dialog: add css and create Ext.fields from input fields
 */
AFW.dialog.refresh=function(){
	//AFW.dialog.autoHeight();
}

/**
 * initialised the given dialog using the stored config options
 * @param {String} dialogname: id of the dialog
 * @param {Object} el:	the element to start animation from
 */
AFW.dialog.init=function(dialogname, el){
	AFW.dialog.el=el;

	var dialog = AFW.dialog.config = AFW.dialog.dialogs[dialogname];
	//console.log("init dialog", dialog);
	
	if(!dialog){
		alert("Dialog " + dialogname + " ist not defined.")
		return false;
	}

	var submitButton = dialog.submitButton;
	var closeButton = dialog.closeButton;
	
	/*	
	AFW.dialog.submitOnEnter = dialog.submitOnEnter;
	*/
	AFW.dialog.submitOnEnter = false;
	if(submitButton && submitButton.submitOnEnter)
		AFW.dialog.submitOnEnter = true;
		
	var dialogobj=AFW.dialog.dialog;
	dialogobj.setTitle(dialog.title);
	/*
	if(dialog.submitButtonText){
		AFW.dialog.submitButton.show();
		AFW.dialog.submitButton.setText(dialog.submitButtonText);
		AFW.dialog.closeButton.setText($L.cancel);
	}
	else {
		AFW.dialog.submitButton.hide();	
		AFW.dialog.closeButton.setText($L.close);
	}

	if(dialog.closeButtonText){
		AFW.dialog.closeButton.setText(dialog.closeButtonText);
	}
	*/
	if(submitButton){
		AFW.dialog.submitButton.show();
		AFW.dialog.submitButton.setText(submitButton.text||$L.save);
		AFW.dialog.closeButton.setText($L.cancel);
	}
	else {
		AFW.dialog.submitButton.hide();	
		AFW.dialog.closeButton.setText($L.close);
	}
	
	if(closeButton){
		AFW.dialog.closeButton.setText(closeButton.text||$L.cancel);
	}
	
	var pos=dialogobj.getEl().getCenterXY(true);
	dialogobj.moveTo(pos[0], 34);
	
	return true;
}

/**
 * utility function to change the title of the dialog after initialisation
 * @param {Object} title
 */
AFW.dialog.setTitle=function(title){
	AFW.dialog.dialog.setTitle(title);
}

/**
 * create one instance of Ext.BasicDialog used for all dialogs
 */
AFW.dialog.initModalDialog=function(){

	var dialog = AFW.dialog.dialog = new Ext.BasicDialog("afw-ext-dialog", { 
        modal:true,
		resizable: false,
		collapsible: false,
		shim: true,
        //shadow:true,
		constraintoviewport: true,
        width:500,
        height:300
	});	
	dialog.addKeyListener(27, AFW.dialog.hide, dialog);
	dialog.addKeyListener(13, function(){
		if(AFW.dialog.submitOnEnter)
			AFW.dialog.submit()
	}, dialog);
	AFW.dialog.submitButton = dialog.addButton($L.save, function(){AFW.dialog.submit()}, dialog);
	AFW.dialog.closeButton = dialog.addButton($L.cancel, AFW.dialog.hide, dialog);
	dialog.on("show", function(){
		if(AFW.dialog.config.focus)
			AFW.dialog.focus(AFW.dialog.config.focus);
		AFW.dialog.onShow();
	});	
};

