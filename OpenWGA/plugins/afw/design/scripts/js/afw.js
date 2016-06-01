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
 * global name space for all JS functions
 */
AFW={
	isSafari: navigator.userAgent.toLowerCase().indexOf("webkit") > -1,
	isIE: navigator.userAgent.toLowerCase().indexOf("msie") > -1,
	isIE7: navigator.userAgent.toLowerCase().indexOf("msie 7") > -1,
	isOpera: navigator.userAgent.toLowerCase().indexOf('opera') > -1
};

/**
 * Messege logger
 */
AFW.Logger = function (id, url){
	// local vars:
	var id=id;	
	var timer=null;
	var status="hide";

	var hide=function(){
		//console.log("Logger.hide()");	
		if(timer){
			window.clearTimeout(timer);
			timer=null;
		}
		var el=document.getElementById(id);
		el.style.display="none";
		el.innerHTML="";
		status="hide";
		//console.log("Logger.hide");
		if(typeof(Ext)!="undefined")
			Ext.getBody().unmask()			
	}

	this.show=function(){
		//console.log("Logger.show(): current status="+this.status);	
		if(status=="show")
			return;
		status="show";
		var el=document.getElementById(id);
		if(el==null)
			return; //alert("no div with id=" + this.id + " found for logger");
		el.style.display="none";
		el.style.position="absolute";
		var w=document.body.offsetWidth;
		var h=document.body.offsetHeight;
		el.style.width=w/2
		el.style.top=h/4
		el.style.left=w/4;
		//this.el=el;
		
		var checkServer=function(){
			//console.log("Logger.checkServer: " + AFW.baseurl + "/html/actionlog");	
			timer=null;
			WGA.ajax.request(AFW.baseurl + "/html/actionlog", null, function(req){
				var el=document.getElementById(id);
				el.innerHTML=req.responseText;
				try{
					var title=el.getElementsByTagName("h1")[0].innerHTML;
					//console.log("callback: " + req.responseText);
					if(title.search(/\S/)>=0){
						el.style.display="block";
						if(typeof(Ext)!="undefined")
							Ext.getBody().mask()			
						timer=window.setTimeout(checkServer, 1000);	
					}
					else hide();
				}catch(e){
					hide();
				}
			})
		}
		timer=window.setTimeout(checkServer, 250);	
	}

}
