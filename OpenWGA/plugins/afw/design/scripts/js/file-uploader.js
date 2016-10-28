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

if(typeof(AFW)=="undefined")
	AFW={};

AFW.FileUploader = function(file, posturl, config){
	
    var xhr = new XMLHttpRequest();
	
	if(config.onProgress){
	    xhr.upload.addEventListener("progress", function(e) {
			if (e.lengthComputable) {
	        	var p = e.loaded / e.total;
	        	config.onProgress(p)
	        }
	    }, false);
	}
	
	xhr.onreadystatechange = function (ev) {  
		if (xhr.readyState == 4) {
			//alert(xhr.status + ":\n" + xhr.responseText);
	     	if(xhr.status == 200){
	     		try{
	      			eval("(response="+xhr.responseText+")")
		      		if(response.msg){
		      			if(config.onError)
		      				config.onError(response.msg);
		      		}
		      		else if(config.onSuccess){
		      			config.onSuccess(response.filename);
		      		}
		      	}
		      	catch(e){
	      			if(config.onError)
	      				config.onError("Unable to evaluate server response as JSON:\n" + e + "\n" + xhr.responseText);
		      	}
	      	}  
	     	else{
      			if(config.onError)
      				config.onError("File Upload Error: " + xhr.status);
	     	}  
	    	delete xhr;   
	  	}  
	};
	
	
	var params = [];
	if(config.urlparams){
		for(i in config.urlparams)
			params.push(i + "=" + encodeURIComponent(config.urlparams[i]))   
	}
	
	var hasParams = (posturl.indexOf("?")>=0)
	var paramsDivider = hasParams ? "&" : "?";
    xhr.open("POST", posturl + (params.length ? paramsDivider+params.join("&") : ""), true);

	try{
	    xhr.setRequestHeader("Content-Type", "application/octet-stream");
	    xhr.send(file);
	}
	catch(e){
		alert("Your Browser does not support file uploads");
	}
	
}
