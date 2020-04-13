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
DBM = function(){

	var history=[];

	Ext.apply(Ext.form.VTypes, {

			url:  function(v) {
		        return /^[\.a-zA-Z0-9_-]+$/.test(v);
		    }
		    ,urlText: 'This field should only contain letters, numbers, ., - and _'
	        ,urlMask: /[\.a-z0-9_-]/i
	
			,aclrole:  function(v) {
		        return /^[\$#\[\]\.a-zA-Z0-9_-]+$/.test(v);
		    }
		    ,aclroleText: 'This field should only contain letters, numbers, ., - and _'
		    ,aclroleMask: /[\$#\[\]\.a-zA-Z0-9_-]/i
	});
	
	// overwrite WGA default ajax action
	var wga_ajax_action = WGA.ajax.action;
	function ajaxaction(actiondef, msg, graydiv){
		if(actiondef.mode!='norefresh'){ 
			DBM.progressbar.show((msg||"loading") + " ...");
			document.body.style.cursor="wait";
			var actionLink = WGA.parseActionLink(actiondef.action);
			var hideProgressBarCallback = {
				destroy: function(){
					DBM.progressbar.hide();
					document.body.style.cursor="";
				}
			};
			WGA.portlet.registerObject(actionLink.portletKey, hideProgressBarCallback);
		}
		actiondef.graydiv=graydiv||false;
		if (wga_ajax_action(actiondef) == false && actiondef.mode!='norefresh') {
			WGA.portlet.unregisterObject(actionLink.portletKey, hideProgressBarCallback);
		};
	}
	WGA.ajax.action = ajaxaction
	
	return{

		showHideElement: function(id, cb){
			var el = Ext.get(id);
			el.setVisibilityMode(Ext.Element.DISPLAY);
			
			if(el.isVisible()){
				if(Ext.isIE){
					el.setStyle("display", "none");
				}
				else el.slideOut("t", {
					//duration: .25,
					callback: function(){
						if(cb)
							cb(false)
					}
				});
			}
			else {
				el.removeClass("x-hidden");
				if(Ext.isIE){
					el.setStyle("display", "block");
				}
				else el.slideIn("t", {
					duration: .25,
					callback: function(){
						if(cb)
							cb(true)
					}
				})
			}
		}
		
		,progressbar: function(){
			var pbar = new Ext.ProgressBar();
			
			return{
				show: function(text){
					if(!pbar.rendered){								
						pbar.render("progressbar");
					}
					else pbar.show();
					pbar.wait();
					pbar.updateText(text);
				}
				
				,hide: function(){
					if(pbar.rendered){
						pbar.reset(true);
					}
				}
				
				// debug:
				,bar: pbar
			}
			
		}()
		
		,switchToObject: function(app, uid, where, implClass){
			var params = {appkey:app, uid:uid, where: where, implClass: implClass};
			history.unshift(params);
			if(history.length>10)
				history.pop();
			WGA.event.fireEvent("PS-app-selected", "*", params)
		}
		,historyBack: function(){
			history.shift();
			var params = history[0];
			//console.log("back to " + params, history)
			if(params)
				WGA.event.fireEvent("PS-app-selected", "*", params)
		}
		
		,ajaxAction: ajaxaction
		
		,callAction: function(action, portletkey, msg){
			ajaxaction({
				action: action,
				id: portletkey
			}, msg, true)
		}

	}
	
}();

DBM.joblog = function(divel, jobname){
	var el = Ext.get(divel);
	var url = DBM.baseurl + "/joblog?name=" + jobname;
	var iframe = el.child("iframe").dom;
	var timer=null;

	var autoUpdate_el;
	if(el.child("#autoUpdate"))
		 autoUpdate_el = el.child("#autoUpdate").dom;	
	
	iframe.onload=function(){
		var win = iframe.contentWindow;
		win.scrollTo(0, win.document.body.scrollHeight);
	}
	
	function showHide(){
		iframe.src="";
		DBM.showHideElement(el, function(show){
			if(show){
				setURL();				
				timer = window.setInterval(refresh, 1000);
			}
			else stopTimer();
		})
	}

	function refresh(){
		if(!iframe.contentWindow.running){
			if(autoUpdate_el)
				autoUpdate_el.checked=false;
			stopTimer();
		}
		if(!autoUpdate_el || autoUpdate_el.checked){
			if(iframe.contentWindow)
				iframe.contentWindow.location.reload();
			else stopTimer();
		}
	}
	
	function setURL(){
		iframe.src=url;
	}
	
	function stopTimer(){
		if (timer){
			window.clearInterval(timer)
			timer=null;
		}
	}
	
	return{
		toggleView: showHide,
		refresh: setURL,
		stopTimer: stopTimer 
	}
}

DBM.dialog = function(title, url, params, animel){
	
	var w = new Ext.Window({
		modal: true,
		title: title,
		width: 850,
		y: 100,
		height: 500,
		autoScroll: true
	})
	w.render(Ext.getBody());
	w.setHeight(Ext.getBody().getHeight()-50);
	w.alignTo(Ext.getBody(), "c-c");
	w.show(animel);
	
	w.load({
		url: url,
		params: params
	})
	
}

DBM.fields = function(){
	
	var fields = [];
	
	return {
		register: function(f){
			fields.push(f);
		}
		,reset: function(){
			fields = [];
		}
		,validate: function(){
			var ret = true;
			for(var i=0; i<fields.length; i++){
				var field = fields[i];
				if(!field.validate())
					ret = false;
				else{
					field.el.dom.value = field.getValue();
				}
			}
			return ret;
		}
	}
	
}();

DBM.iframeManager=function(id, url, portletkey){
	var iframe=document.getElementById(id);
	var url=url;
	
	function resize(h){	
		var el=iframe;
		for(var tmp=el; tmp; tmp=tmp.offsetParent)
		    h -= tmp.offsetTop;		    
		h-=40;
		if(el.height!=h)
			el.height=h;
		if(!el.src)
			el.src=url;
	}	

	function init(){
		if(DBM.viewport)
			resize(DBM.viewport.getSize().height)
		else window.setTimeout(init, 100);
	}

	Ext.EventManager.onWindowResize(function(w,h){
		resize(h);
	})	
	init();
}


DBM.element=function(){
	
	return{
		show: function(id, cb){
			var el = Ext.get(id);
			if(!el.isVisible()){
				el.removeClass("x-hidden");
				el.slideIn("t", {
					callback: function(){
						el.setDisplayed(true);
						if(cb)
							cb();
					}
				})
			}
		}
				
		,hide: function(id, cb){		
			var el = Ext.get(id);
			if(el.isVisible()){
				el.slideOut("t", {
					callback: function(){
						el.setDisplayed(false);
						if(cb)
							cb();
					}
				});
			}
		}

	}
}();


DBM.Action=function(config){
	this.portletkey = config.portletkey;
	DBM.Action.superclass.constructor.call(this, config)
}
Ext.extend(DBM.Action, Ext.Action, {
	setAction: function(action){
		this.setHandler(function(){
			DBM.callAction(action, this.portletkey, "updating configuration");
		})
		this.enable();
	}
});

DBM.actions=(function(){

	var actions={}

	return{
	
		save: function(portletkey){
			var a = actions[portletkey]; 
			if(!a){
				a = new DBM.Action({
					portletkey: portletkey,
				    text: 'save',
				    iconCls: 'action-save-icon',
				    itemId: 'DBM.save'
				})
				actions[portletkey] = a;
				WGA.portlet.registerObject(portletkey, {
					destroy: function(pkey){
						delete actions[pkey];
					}
				});
			}
			return a;
		}	
	}
})();


DBM.dbExplorer={}

// Deprecated.
// should be removed in final version	
switchToObject = DBM.switchToObject

window.addEventListener('load', function () {
	Notification.requestPermission(function (status) {
	    // This allows to use Notification.permission with Chrome/Safari
	    if (Notification.permission !== status) {
			Notification.permission = status;
	    }
	});
});
