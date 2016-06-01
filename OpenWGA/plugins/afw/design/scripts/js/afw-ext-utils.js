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
 * simulate firebug console method for IE
 */

if(document.all){
	console={};
	console.log=console.time=console.timeEnd=console.trace=function(){};		
}

/**
 * global name space for all JS functions
 */
AFW={
	isSafari: navigator.userAgent.toLowerCase().indexOf("webkit") > -1,
	isIE: navigator.userAgent.toLowerCase().indexOf("msie") > -1,
	isIE7: navigator.userAgent.toLowerCase().indexOf("msie 7") > -1,
	isOpera: navigator.userAgent.toLowerCase().indexOf('opera') > -1
};

AFW.util={};

AFW.util.expandElement=function(el, callback){
	Ext.get(el).slideIn("t", {
		scope: el,
		callback: callback
	})
}

AFW.util.collapseElement=function(el, callback){	
	Ext.get(el).slideOut("t", {
		scope: el,
		useDisplay: true,
		callback: callback
	})
}



/* 
 * Singleton-Class to display a self-hiding info-message
 * at center/top of the screen. 
 * Example: AFW.util.InfoWidget.msg('Title', 'Message with {0}...', 'parameter');
 */
AFW.util.InfoWidget = function() {
    var msgCt;

    function createBox(t, s){
    	/*
        return ['<div class="msg">',
                '<div class="x-box-tl"><div class="x-box-tr"><div class="x-box-tc"></div></div></div>',
                '<div class="x-box-ml"><div class="x-box-mr"><div class="x-box-mc"><h3>', t, '</h3>', s, '</div></div></div>',
                '<div class="x-box-bl"><div class="x-box-br"><div class="x-box-bc"></div></div></div>',
                '</div>'].join('');
		*/
        return ['<div class="afw-info-msg">',
                '<h1>', t, '</h1><div>', s, '</div>',
                '</div>'].join('');		
    }
    
    return {
        msg : function(title, format, pause) {
            if(!msgCt){
                msgCt = Ext.DomHelper.insertFirst(document.body, {id:'msg-div'}, true);
            }
            msgCt.alignTo(document, 't-t');
            var s = String.format.apply(String, Array.prototype.slice.call(arguments, 1));
            var m = Ext.DomHelper.append(msgCt, {html:createBox(title, s)}, true);
            m.slideIn('t').pause(pause||3).ghost("t", {remove:true});
        }
    };
}();



AFW.toggleElement=function(el, callback){
	if(el.style.display=="none")
		AFW.util.expandElement(el, callback)
	else AFW.util.collapseElement(el, callback)
}

AFW.callAction=function(action, id, message){

	WGA.ajax.b4post.register(id, function(){
		var el = Ext.get("$ajaxContentDiv_" + id);
		el.update("<b>"+message+"</b><hr size='1'><img src='"+AFW.loadingImg+"'/>&nbsp;" + $L.please_wait + " ...")
		//el.update("<b>"+message+"</b>")
	})
	
	WGA.ajax.action({
		action: action,
		id: id,
		graydiv: false
	});
}

AFW.infoView={
	animating:false,
	
	toggle: function(el){
		if(this.animating)
			return;
		var el=Ext.get(el);
		var title_el=el.child("div");
		var content_el=Ext.get(el.getNextSibling());
		var toolbar_el=content_el.child("div.toolbar");
		this.animating=true;
		
		if(title_el.hasClass("x-layout-expand-north"))
			AFW.util.collapseElement(content_el.dom, function(view){
				title_el.removeClass("x-layout-expand-north");
				title_el.addClass("x-layout-expand-west");
				view.animating=false;
	        }.createCallback(this));
		else AFW.util.expandElement(content_el.dom, function(view){
			title_el.addClass("x-layout-expand-north");
			title_el.removeClass("x-layout-expand-west");
			view.animating=false;
		}.createCallback(this));		
		
	},
	
	expand: function(el, callback){		
		var el=Ext.get(el);
		var title_el=el.child("div");
		var content_el=Ext.get(el.getNextSibling());
		
		if(title_el.hasClass("x-layout-expand-north"))
			return;
			
		this.animating=true;
		AFW.util.expandElement(content_el.dom, function(view){
			title_el.addClass("x-layout-expand-north");
			title_el.removeClass("x-layout-expand-west");
			view.animating=false;
			if(callback)
				callback();
		}.createCallback(this));		
	}	
}
