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
WGAManagement = {
		licensedPlugins: new Array()
};

function calcAbsoluteLeftOffset(elem) {

	var offset = elem.offsetLeft;
	if (elem.offsetParent) {
		offset += calcAbsoluteLeftOffset(elem.offsetParent);
	}
	return offset;

}

function calcAbsoluteTopOffset(elem) {

	var offset = elem.offsetTop;
	if (elem.offsetParent) {
		offset += calcAbsoluteTopOffset(elem.offsetParent);
	}
	return offset;

}

function showDialog(id, e, button) {

	var dialog = document.getElementById(id);
	if (dialog.style.visibility != 'visible') {
		dialog.style.left = calcAbsoluteLeftOffset(button) + button.offsetWidth - dialog.offsetWidth + "px";
		dialog.style.top = calcAbsoluteTopOffset(button) + button.offsetHeight + "px";
		dialog.style.visibility = 'visible';
	}
	else {
		dialog.style.visibility = 'hidden';
	}
}

function performChanges(actionLink, invalidPlugins) {
	
	for (var i=0; i < WGAManagement.licensedPlugins.length; i++) {
		var pluginID = WGAManagement.licensedPlugins[i];
		var pluginLicenseForm = document.forms[pluginID + "_license"];
		var licenseCheckbox = pluginLicenseForm.elements["acceptLicense"];
		if (licenseCheckbox.checked == false) {
			alert("You must agree to the licenses of all pending plugins before continuing. Check the checkbox labeled with 'I accept this license' below each license text to do so.");
			return;
		}
	}

	msg = "Are you sure to commit all pending changes?";
	if (invalidPlugins == true) {
		msg += "\n\nNOTE: There will be invalid plugins in the committed plugin configuration!";
	}

	if (confirm(msg)) {
		callAction(actionLink);
	}
}

function switchDF() {
	
	document.getElementById("dfHide").style.visibility = 'hidden';
	document.getElementById("dfHide").style.display = 'none';
	document.getElementById("dfShow").style.visibility = 'visible';

}

function showDetails(id, workspace) {
	
	var mainDiv = document.getElementById("plugin_" + id);
	var detailsDiv  = document.getElementById("pluginDetails_" + id);
	var menuDiv = document.getElementById("functions_" + id);
	
	
	if (detailsDiv.style.display == 'block') {
		mainDiv.className = (workspace ? "workspacePlugin" : "plugin");

		detailsDiv.style.display = 'none';
		menuDiv.style.visibility = 'hidden';
		
	}
	else {
		mainDiv.className = (workspace ? "workspacePluginNobottom" : "pluginNobottom");
		detailsDiv.style.display = 'block';
	}

}

function menuFunction(element, action) {

	var parentDiv = element.parentNode;
	parentDiv.style.visibility = 'hidden';
	callAction(action);
	
}

function openACL(element, wgaurl, databaseKey){

	var parentDiv = element.parentNode;
	parentDiv.style.visibility = 'hidden';

	if(!databaseKey){
		if( selectedContentDB != null ){
			databaseKey = selectedContentDB;
		}
		else{
			alert("Please select a database first.");
			return false;
		}
	}	
	var parameter="height=500,width=500" 
        +",screenX=0,left=5,screenY=0,top=5"
        +",dependent=0,directories=0"
        +",fullscreen=0,location=0,menubar=0"
        +",resizable=1,scrollbars=1,status=1,toolbar=0";
	open(wgaurl + "/admintml/" + databaseKey + "/manageACL","_blank",parameter);
}

function openScriptConsole(element, wgaurl, databaseKey){

	var parentDiv = element.parentNode;
	parentDiv.style.visibility = 'hidden';

	if(!databaseKey){
		if( selectedContentDB != null ){
			databaseKey = selectedContentDB;
		}
		else{
			alert("Please select a database first.");
			return false;
		}
	}	
	var parameter="height=550,width=600" 
        +",screenX=0,left=5,screenY=0,top=5"
        +",dependent=0,directories=0"
        +",fullscreen=0,location=0,menubar=0"
        +",resizable=1,scrollbars=1,status=1,toolbar=0";
	open(wgaurl + "/admintml/" + databaseKey + "/tmlscriptConsole","_blank",parameter);
}

function csDump(element, wgaurl, databaseKey) {
	
		var parentDiv = element.parentNode;
		parentDiv.style.visibility = 'hidden';
	
		var parameter="height=500,width=500" 
	        +",screenX=0,left=5,screenY=0,top=5"
	        +",dependent=0,directories=0"
	        +",fullscreen=0,location=0,menubar=0"
	        +",resizable=1,scrollbars=1,status=1,toolbar=0";
		open(wgaurl + "/admintml/" + databaseKey + "/csDump","_blank",parameter);
}
