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
 * simulate firebug console method if firebug is not installed
 */

if(typeof console == "undefined"){
	console={};
	console.log=console.time=console.timeEnd=console.trace=function(msg){alert(msg)};
}

/**
 * global name space for all JS functions
 */
BI={
	isSafari: navigator.userAgent.toLowerCase().indexOf("webkit") > -1,
	isIE: navigator.userAgent.toLowerCase().indexOf("msie") > -1,
	isIE7: navigator.userAgent.toLowerCase().indexOf("msie 7") > -1,
	isOpera: navigator.userAgent.toLowerCase().indexOf('opera') > -1
};

BI.menu={};

BI.structkey=null;
BI.contentkey=null;

/**
 * Show selected content in web-panel
 * @param {Object} contentkey
 * @param {Object} structkey
 */
BI.selectContent=function(contentkey, structkey){
	//console.log("BI.selectContent", contentkey);
	if(contentkey==undefined){
		//console.log("BI.selectContent: no contentkey")
		contentkey=BI.contentkey || "";
	}
	if(!structkey)
		structkey=BI.structkey;			
	var contentframe=document.getElementById("web-content");
	if(contentframe){
		BI.setStatus($L.statusbar.loading_page + " " + contentkey + " ...");
		if(contentkey==undefined || contentkey=="nothing")
			contentframe.src=BI.noContentURL+"?structkey="+structkey+"&dbkey="+BI.dbkey;
		else if (contentkey=="")
			contentframe.src=BI.dbpath
		else contentframe.src=BI.dbpath+"/html/default/" + contentkey;
		BI.layout.showWebPanel();
	}
	else alert("no content frame found");
}

BI.findDocument = function(){
	Ext.Msg.show({
		title: $L.goto.title,
		msg: $L.goto.msg,
		prompt:true,
		buttons: Ext.MessageBox.OKCANCEL,
		width: 400,
		fn: function(button, text){
			if(button=='ok' && text!="")
				BI.selectContent(text);
		} 
	})
}


BI.selectDb=function(dbkey, contentkey){
	BI.dbkey=dbkey;
	BI.dbpath=BI.wgaurl+"/"+BI.dbkey;
	BI.selectContent(contentkey);
}

/*******************************************/

/**
 * set text of statis line
 * @param {String} text: HTML text to show in status line
 */
BI.setStatus=function(text){
	try{
		Ext.get("statustext").update(text);
	}catch(e){
		// Ext library may not be loaded yet ...
	}

}

/********/
/**
 * Class BI.panel. Provides method "show" to show or create a new Panel in the center of the main page
 * @param {Object} id
 * @param {Object} title
 * @param {Object} url
 * @param {Object} toolbar_cfg
 */
BI.panel=function(id, title, url, toolbar_cfg){
	var layout;
	var panel;
	
	this.menuToolbarButtons="none";		// the type of menu connected to this panel
	this.toolbar;
	
	this.url=url;
	this.id=id;
	this.title=title;
	this.toolbar_cfg=toolbar_cfg;
	this.defaultParams={};
	this.mode="view";
	
	this.events = {
		"activate": true,
		"removed": true,
		"panelcreated": true,
		"deactivate": true,
		"refresh": true
	}
	
	this.on("activate", function(){
		/**
		 * enable the menu toolbar buttons connected with this panel ("cms", "design", ...)
		 */
		BI.showToolbarButtons(this.menuToolbarButtons)
	})
	
	/**
	 * remove this panel from outside. Mainly used when object is deleted.
	 */
	this.destroy=function(){
		var module = BI.layout.findPanel(this.id);
		if(module)
			BI.layout.getRegion("center").remove(module);
	}
	
	this.activate=function(){
		if(BI.layout.findPanel(this.id))
			BI.layout.showPanel(this.id);
		else this.show();
	}
	
	/**
	 * show this pannel. Create it if not already exists
	 * @param {Object} params: optional new URL-parameter
	 * @param {String} mode: optional new mode (view/edit) of the panel
	 */
	this.show = function(params, mode){
		this.params=params||{};
		this.params.mode = mode||this.mode;
		
		var urlparams={
			dbkey:BI.dbkey,
			contentkey: BI.contentkey,
			structkey: BI.structkey
		}
		for(var p in this.defaultParams)
			urlparams[p]=this.defaultParams[p]
		for(var p in params)
			urlparams[p]=params[p]
	
		var module = BI.layout.findPanel(this.id); 
		if(module){
			if(this.mode!="edit" || confirm($L.cancel_edit)){
				this.mode="view";
				panel.setUrl(BI.wgapath+"/html/"+this.url, urlparams);
				panel.refresh();
			}
			BI.layout.showPanel(this.id);
		}
		else{	// create panel
			var el = Ext.DomHelper.append(document.body, {tag: 'div', id:this.id});
			var el_center = Ext.DomHelper.append(el, {tag: 'div'});
			
			var el_toolbar = Ext.DomHelper.append(el, {tag: 'div'});
			this.toolbar=new Ext.Toolbar(el_toolbar)
			if(this.toolbar_cfg)
				this.toolbar.add(this.toolbar_cfg);
			
		    layout = new Ext.BorderLayout(el, {
					hideOnLayout: true,
		            center: {
		            	autoScroll: true,
		                useShim:true,
		                tabPosition:"top",
		                closeOnTab: true,
		                titlebar:false
					},
		            north: {
						initialSize: 28
					}
			});
		
		    layout.beginUpdate();			
			layout.add('north', new Ext.ContentPanel(el_toolbar));
			panel=layout.add('center', new Ext.ContentPanel(el_center, {
				fitToFrame: false,
				url: BI.wgapath+"/html/"+this.url,
				laodOnce: true,
				params: urlparams
			}));
			panel.getUpdateManager().setRenderer(BI.util.elementUpdater);
			layout.endUpdate();
			
			var p=this.layoutPanel = BI.layout.addPanel('center', new Ext.NestedLayoutPanel(layout, {title:this.title,fitToFrame:true, closable:true}));
			p.on("activate", function(){
				//console.log("layout:panel actiated")
				this.fireEvent("activate", this.params);
			}, this);
			p.on("deactivate", function(){
				this.fireEvent("deactivate", this.params);
			}, this);
			p.region.on("beforeremove", function(region, panel, ev){
				//console.log("region: beforeremove", panel, panel.getId(), ev);
				if(panel.getId()==this.id){
					this.mode="view";
					this.fireEvent("removed", {});
				}
			}, this);
			this.fireEvent("activate", params, true);
			this.fireEvent("panelcreated", params);			
		}
		this.updateTitle(params);
		this.setMode(mode, this.params.id)
		
	}; // end function show()
	
	this.setMode=function(mode, id){
		this.mode=mode||this.mode;
		this.params.mode=this.mode;
		if(this.params.id!=id){
			this.params.id = id;
			this.updateTitle(this.params)
			this.fireEvent("activate", this.params);
		}
		this.fireEvent("refresh", this.mode, this.params);
	}
	this.reload=function(mode){
		this.show(this.params, mode);
	}
	
	this.setTitle = function(title){
		var p = BI.layout.findPanel(this.id);
		if(p)
			p.setTitle(title);
	};
	/*
	 * updateTitle: overwrite this if needed
	 */
	this.updateTitle=function(obj){};
	
	this.enableToolbarButtons = function(enable){
		this.toolbar.items.each(function(item){
			enable ? item.enable() : item.disable()
		})
	};

};
Ext.extend(BI.panel, Ext.util.Observable);


BI.propertiespanel=new BI.panel("property-panel", $L.propertypannel.title, "cms:include-content-properties.int.html", {
	cls: "x-btn-text-icon",
	icon: "../../plugin-wga-app-framework/file/icons/accept.png",
	tooltip: $L.propertypannel.button_tooltip,
	text: $L.save,
	handler: function(m, ev){
		BI.propertiespanel.submit();	// implemented in included TML-module
	}
});
/*
BI.propertiespanel.updateTitle=function(title){
	this.setTitle($L.propertypannel.title + ": " + BI.title)
}
*/
BI.propertiespanel.on("activate", function(params, created){
	//console.log("prop-panel activated", BI.propertiespanel, created)
	if(!created)
		BI.propertiespanel.reload()
});

BI.searchpanel=new BI.panel("search-panel", $L.searchpanel.searchresult, "cms:search-result.int.html", {
	cls: "x-btn-text-icon",
	icon: "../../plugin-wga-app-framework/file/icons/magnifier.png",
	tooltip: $L.searchpanel.click_here_to_search,
	text: $L.searchpanel.button_text,
	handler: function(m, ev){
		BI.dialog.show("search", ev.target);
	}
});

BI.contentpanel=new BI.panel("content-panel", $L.contentviewpanel.title, "system:portlet-includer.int.html");
BI.contentpanel.defaultParams={portlet_tml: "cms:content-views:portlet-contentviews"};

BI.seopanel=new BI.panel("seo-panel", "SEO", "system:portlet-includer.int.html", [
	{
		cls: "x-btn-text-icon",
		icon: "../../plugin-wga-app-framework/file/icons/accept.png",
		tooltip: $L.propertypannel.button_tooltip,
		text: $L.save,
		saveButton: true,
		handler: function(m, ev){
			BI.seopanel.submit();	// implemented in included TML-module
		}
	},
	new Ext.Toolbar.Separator(),
	{
		cls: "x-btn-text-icon",
		icon: "../../plugin-wga-app-framework/file/icons/html.png",
		text: "W3C Validierung",
		handler: function(m, ev){
			BI.dialog.show("w3c-validation", ev.target)
		}
	}
]);
BI.seopanel.defaultParams={portlet_tml: "cms:seo"};
BI.seopanel.on("activate", function(params, created){
	if(!created)
		BI.seopanel.reload()
});

BI.dashboardpanel=new BI.panel("dashboard-panel", "Dashboard", "cms:dashboard.int.html", {
	cls: "x-btn-text-icon",
	icon: "../../plugin-wga-app-framework/file/icons/arrow_rotate_clockwise.png",
	text: "refresh",
	handler: function(m, ev){
		BI.dashboardpanel.reload()
	}
})
BI.dashboardpanel.on("activate", function(params, created){
	if(!created)
		BI.dashboardpanel.reload()
});

BI.infoView={
	animating:false,
	
	toggle: function(el, expand_callback){
		if(this.animating)
			return;
		var el=Ext.get(el);
		var info_el = el.findParent(".info", null, true);
		var title_el=el.child("div");
		var content_el=Ext.get(el.getNextSibling());
		var toolbar_el=content_el.child("div.toolbar");
		this.animating=true;
		
		if(title_el.hasClass("x-layout-expand-north")){
			info_el.addClass("collapsed");
			BI.util.collapseElement(content_el.dom, function(view){
				title_el.removeClass("x-layout-expand-north");
				title_el.addClass("x-layout-expand-west");
				view.animating=false;
				if(expand_callback)
					expand_callback(false)
	        }.createCallback(this));
	    }
		else{
			info_el.removeClass("collapsed");
			BI.util.expandElement(content_el.dom, function(view){
				title_el.addClass("x-layout-expand-north");
				title_el.removeClass("x-layout-expand-west");
				view.animating=false;
				content_el.dom.scrollIntoView(true)
				if(expand_callback)
					expand_callback(true)
			}.createCallback(this));
		}		
		
	},
	
	expand: function(el, callback){		
		var el=Ext.get(el);
		var title_el=el.child("div");
		var content_el=Ext.get(el.getNextSibling());
		
		if(title_el.hasClass("x-layout-expand-north"))
			return;
		
		var info_el = el.findParent(".info", null, true);
		info_el.removeClass("collapsed");
		
		this.animating=true;
		BI.util.expandElement(content_el.dom, function(view){
			title_el.addClass("x-layout-expand-north");
			title_el.removeClass("x-layout-expand-west");
			view.animating=false;
			if(callback)
				callback();
		}.createCallback(this));		
	}	
}

BI.changeWebView = function(cssclass){
	var el = Ext.get("web-content-wrapper");
	el.removeClass(["phone", "phone-landscape", "tablet", "tablet-landscape", "mobile"])
	if(!(cssclass=="browser"))
		el.addClass(["mobile", cssclass]);	
}

/**
 * Ext extensions ...
 */
BI.Ext={};
BI.Ext.BasicDialog=function(el, config){
	BI.Ext.BasicDialog.superclass.constructor.call(this, el, config);
	this.addEvents({
		"closed" : true
	});
	this.close.on("click", function(){
		this.fireEvent("closed", this)
	}, this);
}
Ext.extend(BI.Ext.BasicDialog, Ext.BasicDialog);


/**
 * Public API
 */
window.CM = {

	pageLoaded: function(){
		BI.sitepanel.monitorIframeOnLoad()
	}

}