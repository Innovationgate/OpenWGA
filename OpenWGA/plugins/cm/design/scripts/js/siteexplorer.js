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
if(typeof(BI)=="undefined")
	BI={};

BI.se={
	pageSize: 10
};
BI.se.areas={};
BI.se.structs={};
BI.se.isUpdating = false;
BI.se.keyAfterUpdate=null;

/**
 * register for contentkey-changed events
 */
WGA.event.addListener(
	"panel-siteexplorer",
	"CMS_contentkey_changed", 
	function(e){
		if(BI.se.currentDbKey!=e.params.dbkey){
			BI.se.currentDbKey=e.params.dbkey;
			BI.se.reload();
		}
		else BI.se.selectEntryByKey(e.params.structkey);
	}
);

/**
 * called with this=structkey
 */
BI.se.entryContextMenu=function(ev){
	var xy=ev.xy;
	Ext.lib.Event.stopEvent(ev);
	if(BI.sitepanel.iframe.window.onbeforeunload)
		return;
		
	var structkey=this;
	if(structkey!=BI.structkey){
		// disable all menu items because we will reload the content page
		BI.menu.context.items.each(function(item){
			if(item.observer)
	    		item.disable()
		})			
		BI.selectContent(structkey)
	}
	var h=BI.menu.context.getEl().getHeight();
	var doc_h=Ext.lib.Dom.getViewportHeight();
	if(xy[1]+h>doc_h)
		xy[1]-=h;
	BI.menu.context.showAt(xy);
}

BI.se.addChildToParent = function(parentkey, structkey){

	var parent=BI.se.structs[parentkey];
	if(parent){
		var c = parent.children;
		var found = false;
		for(var i=0; i<c.length; i++){
			if(c[i] == "struct:"+structkey){
				found = true;
				break;
			}
		}
		if(!found)
			parent.children.push("struct:"+structkey);
		//console.log("added " + struct.title + " as new child of " + parent.title + ": " + parent.children.length); 
	}
}


/**
 * timer object to handle dblclick events
 */
BI.se.dblclicktimer=new Ext.util.DelayedTask;
BI.se.DBLCLICK_TIMEOUT=10;

BI.se.finishedUpdate=function(){
	BI.se.isUpdating=false;
	if(BI.se.keyAfterUpdate){
		//console.log("selectAfterUpdate: " + BI.se.keyAfterUpdate)
		BI.se.selectEntryByKey(BI.se.keyAfterUpdate);
		BI.se.keyAfterUpdate = null;
	}
}

/**
 * create the siteexplorer toolbar
 * @param {Ext.element} el 
 */           			
BI.se.createToolbar=function(el){
	var tb = BI.menu.seToolbar = new Ext.Toolbar(el);
    tb.add(
		{	cls: "x-btn-icon",
			icon: "../../plugin-wga-app-framework/file/icons/arrow_rotate_clockwise.png",
			handler: function(){BI.se.reload()},
			tooltip: $L.se.tooltip_reload_se
		},
		"-",
       	{	
       		cls: "x-btn-icon",
       		icon: "../../plugin-wga-app-framework/file/icons/page_copy.png",
       		observer: "copy-page",
       		tooltip: $L.se.tooltip_copy,
       		handler: function(m, ev){
       			BI.clipboard.copy();
       		}
       	},
       	{	
       		cls: "x-btn-icon",
       		icon: "../../plugin-wga-app-framework/file/icons/page_paste.png",
       		tooltip: $L.se.tooltip_paste,
       		observer: "paste-page",
       		handler: function(m, ev){
       			BI.dialog.show("paste-page", ev.target);
       		}
       	},
		"-",
		{	cls: "x-btn-icon",
			icon: "../../plugin-wga-app-framework/file/icons/application.png",
			handler: function(m, ev){
    			BI.dialog.show("create-page", ev.target, {createroot:true});
    		},
			observer: "create-rootpage",
			tooltip: $L.se.tooltip_create_rootpage
		},
		{	cls: "x-btn-icon",
			icon: "../../plugin-wga-app-framework/file/icons/application_double.png",
			handler: function(m, ev){
    			BI.dialog.show("create-page", ev.target);
    		},
			observer: "create-page",
			tooltip: $L.se.tooltip_create_childpage
		},
		"-",
    	{	cls: "x-btn-icon",
			observer: "delete-page",
			icon: "../../plugin-wga-app-framework/file/icons/page_delete.png",
    		handler: function(m, ev){
    			BI.dialog.show("delete-page", ev.target);
    		},
			tooltip: $L.se.tooltip_delete_page
    	},
    	"->",
    	{	cls: "x-btn-icon",			
			icon: "../../plugin-wga-app-framework/file/icons/page_gear.png",
    		handler: function(m, ev){
    			BI.dialog.show("page-settings", ev.target);
    		},
			tooltip: $L.se.tooltip_page_settings
    	}
	);
	return tb;
}

/**
 * Open Area by Element
 * @param {Ext.Element} el
 * @param {Object} options (optional)
 */
BI.se.openArea=function(el, options){
	var area=BI.se.areas[el.id];
	if(!area.isexpanded)
		BI.se.toggleArea(el, options);
	else if(options && options.callback)
		options.callback();
	return area;
}

/**
 * Close Area by Element
 * @param {Ext.Element} el
 * @param {Object} options (optional)
 */
BI.se.closeArea=function(el, options){
	var area=BI.se.areas[el.id];
	if(area.isexpanded)
		BI.se.toggleArea(el, options);
	return area;
}

/**
 * toggle area: expand if collaped, collapse if expanded
 * @param {Ext.Element} el
 * @param {function} callback: (optional) function to be called when all entries are loaded
 * @param {boolean} animation: true if animation is requested, fals otherwise
 */
BI.se.toggleArea=function(el, options){
	options = options||{};
	var callback = options.callback;
	var animation = options.animation;
	
	var area=BI.se.areas[el.id];
	/*
	if(BI.se.currentArea && BI.se.currentArea!=el)
		BI.se.closeArea(BI.se.currentArea);
	*/
	BI.se.currentArea=el;
	if(!area.haschildren)
		return;
		
	area.isexpanded = !area.isexpanded;
	var twisty = el.child(".twisty");
	var areaContainer = el.findParentNode(".area-container", 1, true);
	var children=Ext.get(el.getNextSibling());
	children.setVisibilityMode(Ext.Element.DISPLAY);
	if(animation==undefined)
		animation=true;
	if(area.isexpanded){
		twisty.removeClass("collapsed");
		twisty.addClass("expanded");
		areaContainer.addClass("opened");
		if(animation && BI.animate.siteexplorer)
			BI.util.expandElement(children.dom)
		else children.show();			
	}
	else{
		if(animation && BI.animate.siteexplorer)
			BI.util.collapseElement(children.dom, function(){
				areaContainer.removeClass("opened");
			})
		else children.hide();			
		twisty.removeClass("expanded");
		twisty.addClass("collapsed");
		
	}

	if(!area.isloaded){
		var um=children.getUpdateManager();
		um.showLoadIndicator=false;
		um.loadScripts=true;
		um.update(BI.wgapath+"/html/cms:siteexplorer:rootentries.int.html", {dbkey:BI.dbkey, area:area.name, features:BI.se.features}, function(){
			//console.log("loading rootentries finished", BI.wgapath+"/html/cms:siteexplorer:rootentries", BI.se.structs);
			area.isloaded=true;
			if(callback)
				callback();
		});
	}
	else if(callback)
		callback();
}

/**
 * open children of given struct entry
 * @param {Ext.Element} el
 * @param {function} options
 * @return {object}: the struct data object that was opened
 */
BI.se.openStruct=function(el, options){
	var struct=BI.se.structs[el.id];
	if(!struct.isexpanded)
		BI.se.toggleStruct(el, options);
	else if(options && options.callback)
		options.callback();
	return struct;
}

/**
 * close children f given struct entry
 * @param {Ext.Element} el
 * @param {function} callback
 * @param {boolean} animation
 * @return {object}: the struct data object that was closed
 */
BI.se.closeStruct=function(el, options){
	var struct=BI.se.structs[el.id];
	if(struct.isexpanded)
		BI.se.toggleStruct(el, options);
	else if(options && options.callback)
		options.callback();
	return struct;
}

/**
 * toggle child entries: expand if collaped and collapse if expanded
 * @param {Ext.Element} el
 * @param {function} callback
 * @param {boolean} animation
 */
BI.se.toggleStruct=function(el, options){
	options = options||{};
	var callback = options.callback;
	var animation = options.animation;
	if(!animation==undefined)
		animation=true;
	var pagesize = options.pagesize||BI.se.pageSize;

	var struct=BI.se.structs[el.id];
	if(!struct.haschildren){
		if(callback)
			callback();
		return;
	}
		
	struct.isexpanded = !struct.isexpanded;
	var children=Ext.get(el.getNextSibling());
	children.setVisibilityMode(Ext.Element.DISPLAY);
	
	var twisty = el.child(".twisty");
	if(struct.isexpanded){
		if(animation && BI.animate.siteexplorer)
			BI.util.expandElement(children.dom)
		else children.show();			
		twisty.removeClass("collapsed");
		twisty.addClass("expanded");
	}
	else{
		if(animation && BI.animate.siteexplorer)
			BI.util.collapseElement(children.dom)
		else children.hide();
		twisty.removeClass("expanded");
		twisty.addClass("collapsed");
	}

	if(struct.isexpanded && !struct.isloaded){
		var um=children.getUpdateManager();
		um.showLoadIndicator=false;
		um.loadScripts=true;
		um.update(BI.wgapath+"/html/cms:siteexplorer:childentries.int.html", 
			{dbkey:BI.dbkey, structkey:struct.key, features:BI.se.features,pagesize:pagesize}, 
			function(oElement, bSuccess, oResponse){
				//console.log("loading children finished", BI.wgapath+"/html/cms:siteexplorer:childentries", struct.key);
				//console.log("SE reloaded", oResponse.responseText.length, oResponse);
				struct.isloaded=true;
				if(callback)
					callback();			
			}
		);
	}
	else if(callback)
		callback();
}

BI.se.updateStruct=function(structkey, callback){
	//console.log("udating struct " + structkey);
	var el = Ext.get("struct:"+structkey);
	if(!el){
		//console.log("Unable to find struct " + structkey + " in Siteexplorer");
		return;
	}
	
	var struct=BI.se.structs[el.id];
	var wasexpanded=struct.isexpanded;
	var children = struct.children;
	var wasloaded = struct.isloaded;
		
	var um = el.getUpdateManager();
	um.showLoadIndicator=false;
	um.loadScripts=true;
	um.update(BI.wgapath+"/html/cms:siteexplorer:structentry.int.html", {dbkey:BI.dbkey, structkey:structkey, features:BI.se.features}, function(){
		//console.log("struct " + structkey + " updated: " + el.dom.className);
		// restore status:
		var struct=BI.se.structs[el.id];
		struct.isloaded = wasloaded;
		struct.children=children;
		struct.isexpanded=wasexpanded;
		if(wasexpanded){
			var twisty = el.child(".twisty");
			twisty.removeClass("collapsed");
			twisty.addClass("expanded");
		} 
		if(callback)
			callback();
	});
}

BI.se.updateMore=function(structkey, lastindex){
	var el = Ext.get("struct:"+structkey+"-more");
	el.dom.className="";
	el.dom.id="";
	var um=el.getUpdateManager();
	um.showLoadIndicator=true;
	um.loadScripts=true;
	um.update(BI.wgapath+"/html/cms:siteexplorer:childentries.int.html", 
		{dbkey:BI.dbkey, structkey:structkey, startindex:lastindex, features:BI.se.features,pagesize:BI.se.pageSize}, 
		function(){
			if(BI.se.currentEntry==el)
				BI.se.selectEntryByKey(BI.structkey);
		}
	);		
}	

/**
 * open child entries of given struct
 * @param {Ext.Element} el
 * @param {function} callback
 */
BI.se.openEntry=function(el, options){
	var e_type=el.id.split(":");
	if(e_type[0]=="area")
		return BI.se.openArea(el, options);
	else if(e_type[0]=="struct")
		return BI.se.openStruct(el, options);
}

/**
 * Event callback when user clicks on a twisty or area entry or dblclicks on a tree entry
 * @param {Ext.EventObject} ev: the Event object
 * @param {Ext.Element} el: the Ext.Element object that was clicked
 */
BI.se.toggleEntryEvent=function(ev, el){
	// use timer to prevent to act twice on dblclick
	// console.log("want handleClickEvent:", ev, el);
	BI.se.dblclicktimer.delay(BI.se.DBLCLICK_TIMEOUT, function(){
		BI.se.toggleEntry(this);
	}, this);
	ev.stopEvent();
}

/**
 * Event callback when user clicks on a tree entry
 * @param {Ext.EventObject} ev: event object
 * @param {Ext.Element} el: the element that was cliched
 */
BI.se.selectEntryEvent=function(ev, el){	
	// use timer to prevent to act twice on dblclick	
	BI.se.dblclicktimer.delay(BI.se.DBLCLICK_TIMEOUT, function(el){
		//console.log("handle selectEntryEvent:", this);
		if(BI.se.currentEntry==this)
			return;	// console.log("entry already selected");
		
		var e_type=this.id.split(":");
		if(e_type[0]=="struct"){
			var contentkey = BI.se.structs[this.id].bestContentKey;
			BI.selectContent(contentkey||"nothing", e_type[1]);
		}
		
	}, this);
}

/**
* toggle tree entry based on Ext.Element
* @param {Ext.Element} el: the Ext.Element object that was clicked
* @param {function} callback: function called when action is completed
*/
BI.se.toggleEntry=function(el, options){
	//console.log("BI.se.toggleEntry", el)
	var e_type=el.id.split(":");
	if(e_type[0]=="area")
		BI.se.toggleArea(el, options);
	else if(e_type[0]=="struct")
		BI.se.toggleStruct(el, options);
}

/**
 * Select tree entry based on Ext.Element. This method does not expand any parent entries
 * @param {Ext.Element} el
 * @param {String} title (optional): title of the entry
 */
BI.se.selectEntry=function(el, title){
	
	el.dom.scrollIntoView(false)
	
	if(title)
		el.child(".entrytitle").update(BI.util.encode(title));

	if(BI.se.currentEntry==el)
		return //console.log("entry already selected");
	//console.log("selcted entry: " + el.id + "/" + title) 
	if(BI.se.currentEntry)
		BI.se.currentEntry.toggleClass("selected");
	el.toggleClass("selected");
	BI.se.currentEntry=el;	
}

/**
 * Select tree entry based on structkey
 * called from outside the sitexplorer to select an entry
 * if structkey is not avaliable it reads the stuct path from server and opens all entries
 * @param {String} key: the structkey to select
 */
BI.se.selectEntryByKey=function(key){
	//console.log("selectEntryByKey: "+key)
	if(BI.se.isUpdating){
		//console.log("selectEntryByKEy: busy. KeyAfterUpdate: " + key)	
		return BI.se.keyAfterUpdate=key;
	}

	if(BI.se.currentEntry)
		BI.se.currentEntry.removeClass("selected");
	BI.se.currentEntry=null;
	
	if(!key || key=="undefined"){
		return;
	}
	//console.log("BI.language=", BI.language, BI.se.currentLanguage)
	if(BI.language && BI.language!=BI.se.currentLanguage){
		BI.se.currentLanguage=BI.language;
		return BI.se.reload(key);		// if language is diffenrent, reload the whole tree
	}
		
	var struct=BI.se.structs["struct:"+key];
	if(!struct){
		//console.log("key not found: load from server", key);
		return getStructPathFromServer(key);	// load struct-path from server and open all entries in ajax callback
	}
	var el=Ext.get("struct:"+key);
	if(!el)
		return alert("structentry not found in tree");
	BI.se.selectEntry(el, BI.title);
	BI.se.updateStruct(key);
	
	var parent=struct.parent;
	while(parent){
		//console.log("open parent entry: " + parent);
		var el=Ext.get(parent)
		var entry=BI.se.openEntry(Ext.get(parent));
		parent=entry.parent;
	}

	// private function: load struct-path from server and open all entries
	function getStructPathFromServer(key){
		BI.se.isUpdating=true;
		var path;
		var url="./cms:siteexplorer:get-struct-path.int.html?dbkey="+BI.dbkey+"&structkey="+key;
		Ext.Ajax.request({
			url:url,
			method: "get",
			success: function(o) {
				eval(o.responseText);	// saves result as array in var path. See cms:siteexplorer:get-struct-path.tml
				//alert(path);
				openPath();		
			}, 
			failure: function(o) {
				BI.se.finishedUpdate();
				alert("BI.se.selectEntryByKey / getStructPathFromServer: ajax load error")}
		});

		var last_opened_el;
		function openPath(){
			//console.log("openPath: ", path);
			if(path.length==0)
				return;
			var entry=path.pop();
			if(path.length==0){
				BI.se.finishedUpdate();
				var el = Ext.get(entry);
				//console.log("all opend, selecting entry", entry, el);				
				try{
					if(el)
						BI.se.selectEntry(el);
					else{
						var el = Ext.get(last_opened_el.id+"-more");
						//console.log("path.length=0. el " + entry + " not found", last_opened_el.id, el);
						BI.se.selectEntry(el);
					}
				}
				catch(e){
					//console.log(e);
				}
			}

			var el = Ext.get(entry);
			if(el){				
				BI.se.openEntry(el, {
					callback: function(){
						last_opened_el = el;
						openPath();		// open next entry
					}
				});
			}
			else{
				BI.se.finishedUpdate();
				try{
					var el = Ext.get(last_opened_el.id+"-more");
					BI.se.selectEntry(el);
				}
				catch(e){}
				return;		// entry not found - may be bc. entry ha not readably content
			}
		}
	}
}

/**
 * reload siteexplorer
 * @param {String} structkey: (optional) the structkey of the entry to be selected after reload
 * @param {function} callback: (optional) called when reload is complete
 */
BI.se.reload=function(structkey, callback){
	//console.log("reload SE", structkey);
	BI.se.isUpdating=true;

	BI.se.areas={};
	BI.se.structs={};
	BI.se.currentEntry=null;
	BI.se.currentArea=null;
	var um=BI.se.rootEl.getUpdateManager();
	um.loadScripts=true;
	var structkey = structkey || BI.structkey;
	//console.log("siteexplorer reloads with key " + structkey);
	um.update(BI.wgapath+"/html/cms:siteexplorer:areas.int.html", {dbkey:BI.dbkey, structkey:structkey}, function(){
			//console.log("siteexplorer reloaded with key " + structkey);
			BI.se.finishedUpdate();
			if(BI.language)
				BI.se.currentLanguage=BI.language;
			BI.se.selectEntryByKey(structkey);
			if(callback)
				callback();
	});
	
}

/**
 * reload part of the tree
 * @param {String} structkey: the structkey of the parent that should be reloaded
 * @param {Object} callback: (optional) called when reload is complete
 */
BI.se.reloadParent=function(structkey, selectKeyAfterReload, callback){
	
	var struct=BI.se.getStruct(structkey);
	if(!struct){
		if(callback)
			callback();
		return BI.se.selectEntryByKey(selectKeyAfterReload);
	}
	if(struct.isRoot)
		return BI.se.reload(selectKeyAfterReload, callback);		// reload whole siteexplorer
	
	var parentkey=BI.se.structs[struct.parent].key;
	return BI.se.reloadStruct(parentkey, {
		callback:callback,
		selectKeyAfterReload:selectKeyAfterReload
	});
	
}

BI.se.reloadStruct=function(structkey, options){
	options = options||{};
	options.selectKeyAfterReload = options.selectKeyAfterReload||BI.structkey;
	
	var struct=BI.se.getStruct(structkey);		//BI.se.structs["struct:"+structkey];
	//console.log("reload struct " + struct.title);
	var structEl=Ext.get("struct:"+structkey);
	if(!structEl){
		if(options.callback)
			options.callback();
		return BI.se.selectEntryByKey(options.selectKeyAfterReload);
	}
	BI.se.closeStruct(structEl, {animation:false});		// DON'T USE ANIMATION HERE beauce we will reopen this in a moment
	
	var ce = BI.se.currentEntry
	if(BI.se.currentEntry)
		BI.se.currentEntry.toggleClass("selected");
	BI.se.currentEntry=null;
	
	removeChildren(struct);
	
	BI.se.updateStruct(structkey, function(){
		BI.se.openStruct(structEl, {
			pagesize:options.pagesize,
			callback: function(){
				BI.se.selectEntryByKey(options.selectKeyAfterReload);
				if(options.callback)
					options.callback();
			}
		});
	});
	
	function removeChildren(struct){
		//console.log("removeChildren", struct.title, struct.children)
		for(var s=0; s<struct.children.length; s++){
			var childkey=struct.children[s];
			var child=BI.se.structs[childkey];
			removeChildren(child);
			delete BI.se.structs[childkey];
		}
		struct.children=[];
		struct.isloaded=false;
	}
	
}


/**
 * check if 'parent' is a parent of 'child'
 * @param {Object} parent
 * @param {Object} child
 */
BI.se.isParent=function(parent, child){
	while(child){
		if(child.key==parent.key)
			return true;	
		child=BI.se.structs[child.parent]
	}
	return false;
}

/**
 * get struct object by structkey
 * @param {String} structkey: the structkey
 */
BI.se.getStruct=function(structkey){
	return BI.se.structs["struct:"+structkey];
}

/**
 * Drag&Drop for siteexplorer
 */

/**
 * register all child element with class "title" or "title-no-edit" for drag&drop operation.
 * @param {Ext.Element} el: the dragable element
 * @param {Object} data: custom data object used to handle drop by the target
 */
BI.se.registerDD=function(el, data){
	//console.log("d&d before registered: " + data.type + ":" + data.name);
	//var els=Ext.get(Ext.query(".title, .title-no-edit", el.dom));
	var els=Ext.get(Ext.query(".entrytitle", el.dom));
	els.each(function(){
		Ext.dd.Registry.register(this.dom, data);
		//console.log("d&d registered: " + data.type + ":" + data.name);
	})
}

/**
 * check if source struct may be dropped to target struct
 * @param {Object} sourceStruct: the struct data object of the struct that is draged
 * @param {Object} targetStruct: the struct data object of the target of the drag operation
 */
BI.se.isAllowedDropPosition=function(sourceStruct, targetStruct){
	if(!BI.mayMoveStructEntries)
		return false;
	if(targetStruct.type=="area"){
		// dropped on area
		//console.log("dropped on area", targetStruct.name)
		if(targetStruct.isTemplateArea)
			return false;		// #00001929: don't drop to template area
		if(!targetStruct.mayEditChildren)
			return false;	// user is not allowed to edit something in this area
	
		switch(sourceStruct.ctPositioning){
			case BI.POSITIONING_FIXEDPARENTS:
				return false;
				break;
			case BI.POSITIONING_FIXEDPARENTTYPES:
				return false;
				break;
			case BI.POSITIONING_CHILDENTRIES:
				return false;
				break;
		}
		return true;
	}
	
	if(sourceStruct.key==targetStruct.key)
		return false;		// don't move to yourself

	if(BI.se.isParent(sourceStruct, targetStruct))
		return false;		// don't move to a children or childrens children	

	if(!targetStruct.mayEditChildren)
		return false;		// user is not allowed to create children.
		
	var allowedPositions = sourceStruct.allowedPositions.toLowerCase().split("|");
	switch(sourceStruct.ctPositioning){
		case BI.POSITIONING_EVERYWHERE:
			return true;
		case BI.POSITIONING_FIXEDPARENTS:
			if(allowedPositions.indexOf(targetStruct.key)>=0){
				//console.log("check allowed: ", targetStruct.key, allowedPositions);
				return true;
			}
			break;
		case BI.POSITIONING_FIXEDPARENTTYPES:
			if(allowedPositions.indexOf(targetStruct.contenttype)>=0)
				return true;
			break;
		case BI.POSITIONING_ROOTENTRIES:
			if(targetStruct.isRoot)
				return true;
			break;
		case BI.POSITIONING_CHILDENTRIES:
			if(!targetStruct.isRoot)
				return true;
			break;
	}
	return false;
}

/*
 * init drag&drop on document ready.
 * Create dd.DragZone and dd.DropZone objects and overwrite methods.
 */
BI.se.initDragDrop=function(){
	/**
	 * create DragZone and overwrite methods
	 */
	var dragzone=new Ext.dd.DragZone("siteexplorer-body" , {containerScroll: true, group: "se.tree-entry"});
	
	/**
	 * Called on MouseDown. chech if movedEntry is an area.
	 * @param {Object} movedEntry
	 * @param {Object} e: event object
	 * @return {object}: true, if drag is OK, false if not
	 */
	dragzone.onBeforeDrag=function(movedEntry, e){
		if(movedEntry.type!="struct")
			return false;		// only move structentries
		if(movedEntry.isInTemplateArea)
			return false;		// #00001929: don't move from template area
		return true;
	}

	/**
	 * called on start drag
	 * gray out all not allowed drop positions
	 * update the proxy object with content of drag element
	 * Store movedEntry to be checked in ajax reloads to gray all new added entries
	 */
	dragzone.onStartDrag=function(){
		var movedEntry=BI.se.movedEntry=dragzone.dragData;
		for(var e in BI.se.structs){
			var targetEntry=BI.se.structs[e];
			if(!BI.se.isAllowedDropPosition(movedEntry, targetEntry))
				Ext.get("struct:"+targetEntry.key).setOpacity(.5)
		}
		BI.sitepanel.mask();
		this.proxy.update(BI.se.movedEntry.title);
		return true;
	}

	/**
	 * called when drag ends.
	 * un-gray all entries
	 * @param {Object} movedEntry
	 */
	dragzone.onEndDrag=function(movedEntry){
		BI.sitepanel.unmask();
		if(movedEntry.type=="area")
			return true;
		BI.se.movedEntry=null;
		for(var e in BI.se.structs){
			Ext.get("struct:"+BI.se.structs[e].key).clearOpacity()
		}
		return true;	
	}
	
	/*
	 * create DropZone and overwrite methods
	 */
	var ddExpandTimer=new Ext.util.DelayedTask;
	var dropzone=new Ext.dd.DropZone("siteexplorer-body", {group: "se.tree-entry"});
	/**
	 * called when an element is dropped
	 * Check, if drop is allowed and open dialog
	 * @param {Object} targetEntry
	 * @param {Object} dd
	 * @param {Object} e
	 * @param {Object} movedEntry
	 */
	dropzone.onNodeDrop=function(targetEntry, dd, e, movedEntry){
		if(BI.se.isAllowedDropPosition(movedEntry, targetEntry))
			BI.dialog.show('move-page', Ext.get(targetEntry.ddel),
				targetEntry.type=="struct" ? {fromstructkey:movedEntry.key, tostructkey:targetEntry.key}:{fromstructkey:movedEntry.key, toarea:targetEntry.name}
			);
        return true;
	}
	/**
	 * called while a drag-element is over a drop element.
	 * check if drop is allowed and return this.dropAllowed or this.dropNotAllowed
	 * @param {Object} targetEntry
	 * @param {Object} dd
	 * @param {Object} e
	 * @param {Object} movedEntry
	 */
	dropzone.onNodeOver=function(targetEntry, dd, e, movedEntry){
		//console.log("onNodeOver", targetEntry, dd, e, movedEntry);
		if(BI.se.isAllowedDropPosition(movedEntry, targetEntry))
			return this.dropAllowed;
		return this.dropNotAllowed;
	}
	/**
	 * called when a drag element enters a drop element
	 * start timer to expand entries after sime time
	 * @param {Object} targetEntry
	 * @param {Object} dd
	 * @param {Object} e
	 * @param {Object} movedEntry
	 */
	dropzone.onNodeEnter=function(targetEntry, dd, e, movedEntry){
		Ext.get(targetEntry.ddel).addClass("dragover");
		
		if(targetEntry.type=="drop-pos-before" || targetEntry.type=="drop-pos-after")
			Ext.get(targetEntry.ddel).setStyle("backgroundColor", "red");
		
		if(!targetEntry.haschildren || targetEntry.isexpanded)
			return;
		var el = Ext.get(targetEntry.type=="area" ? "area:"+targetEntry.name : "struct:"+targetEntry.key);
		//console.log("nodeEnter", el);
		ddExpandTimer.delay(1500, function(){
			BI.se.openEntry(this);
		}, el);	
	}
	/**
	 * called when drop element leaves a drop element.
	 * cancel timer started in onNodeEnter()
	 * @param {Object} targetEntry
	 * @param {Object} dd
	 * @param {Object} e
	 * @param {Object} movedEntry
	 */
	dropzone.onNodeOut=function(targetEntry, dd, e, movedEntry){
		ddExpandTimer.cancel();
		Ext.get(targetEntry.ddel).removeClass("dragover");
		if(targetEntry.type=="drop-pos-before" || targetEntry.type=="drop-pos-after")
			Ext.get(targetEntry.ddel).setStyle("backgroundColor", "");
	}
};

Ext.get("init").update("Siteexplorer loaded ...")
