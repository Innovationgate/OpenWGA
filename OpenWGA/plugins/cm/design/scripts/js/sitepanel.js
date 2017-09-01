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
 * handle site panel containing the website in an iframe
 */

Ext.get("init").update("loading Sitepanel ...");

BI.sitepanel={};
BI.sitepanel.iframe={};
BI.sitepanel.actionspanel={
	reload: function(){}
};

BI.sitepanel.layout=function(){
	// locals:
	var layout;
	var hasLayout=false;
	
    return {
		init : function(){
		
			// create nested layout for site-panel:
			layout=new Ext.BorderLayout("site-panel", {
                hideOnLayout: true,
                center: {
                    autoScroll:true,
                    closeOnTab: true,
                    titlebar:false
                },
                east: {
                	split:true,
                    initialSize: 260,
                    minSize: 260,
                    maxSize: 400,
                    autoScroll:false,
                    collapsible:true,
                    useShim:true,
                    showPin: true,
                    animate: BI.animate.panels,
                    title: "Content Management"
                }
                
                /*
                ,south: {
                	initialSize: 150,
                	title: "Attachments ...",
                	split:true,
                	showPin: true,
                	animate: BI.animate.panels,
                    collapsible:true,
                    collapsed:true
                }*/

            });
            
            /*
			layout.add('center', new Ext.ContentPanel('web-content-wrapper', {
				title: 'Website "' + BI.dbtitle + '"', 
				fitToFrame: false
			}));
			*/

			var inner_layout = new Ext.BorderLayout("inner-site-panel", {
				north: {
					initialSize: 28
				},
				center: {					
				}
			}) 
			inner_layout.add('center', new Ext.ContentPanel('web-content-wrapper', {
				title: 'Website "' + BI.dbtitle + '"', 
				fitToFrame: false
			}));
			inner_layout.add('north', new Ext.ContentPanel('history-panel', {}));
			
			layout.add('center', new Ext.NestedLayoutPanel(inner_layout, "some title"))
			layout.add('east', new Ext.ContentPanel('actions-panel', {title: 'Aktionen', fitToFrame: true}));
			
			layout.endUpdate();
			hasLayout=true;
		},
		
		getLayout: function(){
			return layout;
		},

		addPanel: function(reg, panel){
			layout.add(reg, panel);
			return panel;
		},
		
		
		findPanel: function(id){
			return layout.findPanel(id);
		},
		
		showPanel: function(id){
			var panel=layout.findPanel(id)
			if(panel && !panel.active)
				layout.getRegion('center').showPanel(id);
		},
		
		showWebPanel: function(){
			this.showPanel("web-content-wrapper");
		},
		
		getRegion: function(reg){
			return layout.getRegion(reg);
		},
		
		hasLayout: function(){
			return hasLayout;
		}
	}				

}();

/**
 * check if the website has create-areas
 */
BI.sitepanel.hasCreateAreas=function(){
	var win=BI.sitepanel.iframe.window;
	if(win){
		var doc=win.document;
		return Ext.query(".BI-create", doc.body).length>0;
	}
	else return false;
}

/**
 * show or hide create-areas of the website
 * @param {Boolean} show
 */
BI.sitepanel.showCreateAreas=function(show){
	var win=BI.sitepanel.iframe.window;
	var doc=win.document;
	
	if(show){
		Ext.get(Ext.query(".createpage", doc.body)).setDisplayed("block");
		Ext.get(Ext.query(".BI-create", doc.body)).each(function(){

			this.setStyle({
				position: "relative",
				zIndex: 10001
			})
			this.show(true);
			
			var info_el=this.child("span");
			eval("var info=" + info_el.dom.innerHTML);
			this.on("click", function(ev, target, options){
				if(this.area)
					BI.dialog.show("create-page", null, {area: this.area, pagetype:this.pagetype||""})
				else BI.dialog.show("create-page", null, {parentkey:this.structkey, pagetype:this.contenttype||""})
			}, info)
		})
		BI.sitepanel.contentMask.show();
		WGA.event.fireEvent("CMS_createpage", "sitepanel", {mode:'show'});		
	}
	else{
		Ext.get(Ext.query(".createpage, .BI-create", doc.body)).setDisplayed(false)
		BI.sitepanel.contentMask.hide();
		WGA.event.fireEvent("CMS_createpage", "sitepanel", {mode:'hide'});
	}

}

BI.sitepanel.contentMask = function(){

	function resizeContentMask(){
		var win=BI.sitepanel.iframe.window;
		this.setSize(BI.util.dom.getDocumentWidth(win), BI.util.dom.getDocumentHeight(win));
	}
	
	return {
		show: function(){
	
			var win=BI.sitepanel.iframe.window;
			var doc=win.document;
		
			if(!BI.sitepanel.iframe.mask){
				var el=doc.createElement("div");
				doc.body.appendChild(el);
				var mask = BI.sitepanel.iframe.mask = Ext.get(el);
				mask.setStyle({
					zIndex:10000,   
				   	position: "absolute",
				   	top: 0,
				   	left: 0,
				   	backgroundColor: "black"
				})
				mask.enableDisplayMode('block');
			}
			
			var mask=BI.sitepanel.iframe.mask;
			mask.setSize(BI.util.dom.getDocumentWidth(win), BI.util.dom.getDocumentHeight(win));
			mask.setOpacity(0.65);
			mask.show();
			Ext.EventManager.onWindowResize(resizeContentMask, mask);
		}
	
		,hide: function(){
			var mask=BI.sitepanel.iframe.mask;
			mask.hide();
			var win=BI.sitepanel.iframe.window;
			Ext.EventManager.removeResizeListener(resizeContentMask, mask);
		}
	}
	
}()

/**
 * show all editable items on the page
 */
BI.sitepanel.initItemEditors=function(){
	BI.sitepanel.editor=null;		
	
	var iframe = BI.sitepanel.iframe;
	var win=iframe.window;
	var doc=win.document;
	
	// clean items
	// RTF items may contain code that leads to chaos. Mainly created through copy&paste.
	// We remove all DOM elements $(".WGA-Item .WGA-Item") here:
	Ext.get(Ext.query(".WGA-Item .WGA-Item", doc.body)).each(function(){
		this.remove();
	})
	
	var els=Ext.get(Ext.query(".WGA-Item", doc.body));
	var form_error_handler;
	
	BI.sitepanel.hasCustomFormErrors=false;	
	els.each(function(domel, els, index){
		try{
			var item=Ext.get(this.dom);
			
			if(item.dom.getAttribute("cm.item")=="init"){
				//item already initialized
				return;
			}
	
			/* check valid WGA-Item structure:
			 * Expected:
			 * 	<span class="WGA.Item">
			 *		<span class="WGA-Item-Info">...</span>
			 *		<span class="WGA-Item-Label">...</span>
			 *		<span class="WGA-Item-Edit">...</span>
			 *		<span class="WGA-Item-Value">...</span>
			 *	</span>
			 */
			var item_info_el=item.child(".WGA-Item-Info");
			var item_label_el=item.child(".WGA-Item-Label");
			var item_value_el=item.child(".WGA-Item-Value");
			var item_edit_el=item.child(".WGA-Item-Edit");
	
			if(!(item_info_el && item_label_el && item_value_el && item_edit_el))
				return; 

			/* 
				Structure seems to be OK
			*/
			
			var params=item_info_el.dom.innerHTML.replace(/\s/g, "").split("|");
	
			item.dom.setAttribute("cm.item", "init")
	
			switch(params[1]){
				case "rtf":
				case "image":
				case "textblock":
					item.setStyle("display", "block");
					break;
				default:
					item.setStyle("display", "inline");
					break;
			}
			
			if(params[1]=="image")
				BI.sitepanel.initImgDropHandler(params[0], item)
	
			try{
				var editor_options=item.child(".WGA-Editor-Options").dom.innerHTML.replace(/[\n\r]/g, "");
				var wga4_editor_options=item.child(".WGA4-Editor-Options").dom.innerHTML.replace(/[\n\r]/g, "");
			}
			catch(e){
				// options may not exist
			}			
			
			// show item-label if item is empty:
			if(item_value_el.dom.innerHTML==""){
				if(params[1]=="image"){
					item_label_el.setStyle({
						display: "block",
						padding: "30px 5px"
					})
				}
				else if(params[1]=="rtf"){
					item_label_el.setStyle({
						display: "block",
						padding: "30px 5px"
					})
				}
				else if(params[1]=="textblock"){
					item_label_el.setStyle({
						display: "block"
					})
				}
				else{
					item_label_el.setStyle({
						display: "inline"
					})
				}
			}
			
			function mover(ev){
				item_edit_el.update($L.edit_field);
			}
			function mout(ev){
				item_edit_el.update("");
			}
			
			item_edit_el.dom.onmouseover=mover;
			item_edit_el.dom.onmouseout=mout;
			
			item.dom.onmouseover=function(){
				item.addClass("WGA-Item-hover");
				item_edit_el.setDisplayed(true);
			};
			item.dom.onmouseout=function(){
				item.removeClass("WGA-Item-hover");
				if(!BI.sitepanel.editItemsVisible){
					item_edit_el.setDisplayed(false);
				}
			};

			item_edit_el.dom.onclick = function(){
				BI.sitepanel.openItemEditor.call({el:item, item:params[0], editor:params[1], options:editor_options, wga4options:wga4_editor_options});
			}
			item.dom.ondblclick=function(){
				BI.sitepanel.openItemEditor.call({el:item, item:params[0], editor:params[1], options:editor_options, wga4options:wga4_editor_options});
			}
			
			var error_item_el = item.child(".WGA-Custom-Form-Errors");
			if(error_item_el){
				// item with editor="custom" had validation errors:
				form_error_handler = BI.sitepanel.openItemEditor.createDelegate({el:item, item:params[0], editor:params[1]})
				BI.sitepanel.hasCustomFormErrors=true; 
			}
		}
		catch(e){
			//console.log(e);
		}
	})
	
	if(form_error_handler){
		form_error_handler();
	}
	else if(BI.sitepanel.editItemsVisible)
		BI.sitepanel.showItemEditors(true);		
		
}

BI.sitepanel.removeEditOnOver=function(){
	var win=BI.sitepanel.iframe.window;
	var doc=win.document;
	var els=Ext.get(Ext.query(".WGA-Item", doc.body));
	
	els.each(function(el, els, index){
		el.removeClass("WGA-Item-hover");
		el.dom.onmouseover=null
		el.dom.onmouseout=null
		el.dom.ondblclick=null
	})
}

/**
 * show all editable items on the page
 */
BI.sitepanel.editItemsVisible = true;	// Default: show all edit items
BI.sitepanel.showItemEditors=function(show, dont_save_state){
	try{
		var win=BI.sitepanel.iframe.window;
		var doc=win.document;
		var els=Ext.get(Ext.query(".WGA-Item-Edit", doc.body));
		
		els.each(function(el, els, index){
			el.setVisibilityMode(Ext.Element.DISPLAY);
			if(show)
				el.setDisplayed(true);
			else el.setDisplayed(false);
		})
	}
	catch(e)
	{
		//console.log(e);
	}
	if(!dont_save_state)
		BI.sitepanel.editItemsVisible = show;	
}	

BI.sitepanel.togglePreview = function(){
	BI.sitepanel.showItemEditors(!BI.sitepanel.editItemsVisible)
}

BI.sitepanel.openItemEditor=function(){

	if(this.editor=="upload"||this.editor=="file")
		return BI.dialog.show("upload", this.el, {type:'content', key:BI.contentkey});
	if(this.editor=="image")
		return BI.dialog.show("image-item-editor", this.el, {item: this.item});

	this.el.child(".WGA-Item-Edit").hide();	// hide edit item to avoid double clicks
	
	BI.sitepanel.removeEditOnOver();
	BI.sitepanel.showItemEditors(false, true);
	
	var item_label_el=this.el.child(".WGA-Item-Label");
	item_label_el.dom.style.display="none";

	WGA.event.dispatch({
		name:"CMS_item_edit",
		source:"portlet-website-observer",
		params:{
			contentkey: BI.contentkey, 
			item: this.item,
			editor: this.editor,
			options: this.options,
			wga4options: this.wga4options
		}
	});
	WGA.event.fireEvents();		

	if(BI.sitepanel.iframe.window.WGA){
		BI.sitepanel.iframe.window.WGA.event.dispatch({
			name:"CMS_item_edit",
			source:"portlet-website-observer",
			params:{
				contentkey: BI.contentkey,
				item: this.item,
				editor: this.editor,
				options: this.options,
				wga4options: this.wga4options
			}
		});
		BI.sitepanel.iframe.window.WGA.event.fireEvents();		
	}
	
}

/**
 * main event handler called when a new page is loaded into the website-s iframe
 * Store WGA.contentinfo into local vars, update the status bar and fire event "CMS_monitor_contentkey_changed"
 */
BI.sitepanel.monitorIframeOnLoad=function(){
	
	if(BI.sitepanel.ddhandler){
		BI.sitepanel.ddhandler.destroy();
		BI.sitepanel.ddhandler = null;
	}
	
	if(BI.contenteditor && BI.layout.getWebPanel().active){
		BI.showToolbarButtons("cms");
	}
	BI.contenteditor=null;
	
	var scrollTo;
	if(BI.sitepanel.iframe && BI.sitepanel.iframe.scrollTo)
		scrollTo = BI.sitepanel.iframe.scrollTo

	var pageYOffset = BI.sitepanel.iframe && BI.sitepanel.iframe.pageYOffset

	BI.sitepanel.iframe = {};
	var iframe=BI.sitepanel.iframe;
	iframe.el=Ext.get("web-content");
	iframe.window=iframe.el.dom.contentWindow;

	try{
		if(iframe.window.WGA_cm_init)
			return;

		iframe.document=iframe.window.document;

		var info=iframe.window.WGA.contentinfo;
		var contentkey=info.contentkey;
		var structkey=info.structkey;
		var language=info.language;
		BI.title=Ext.util.Format.htmlEncode(info.title) || "";
		BI.dbkey=info.dbkey;

		BI.rtftoolbar.hide();			

		if(iframe.window.WGA.ddhandler)
			initCustomDDHandler(iframe)			

	}
	catch(e){
		// will be executed, if page is no WGA page
		// loacal vars will be undefined in this case
		BI.sitepanel.no_wga_page = e.message;
	}
	
	BI.contentkey=contentkey;				
	BI.structkey=structkey;
	BI.language=language;

	BI.layout.getWebPanel().setTitle(BI.dbkey);
	
	WGA.event.fireEvent("CMS_monitor_contentkey_changed", "BI.sitepanel.monitorIframeOnLoad", {
		dbkey: BI.dbkey,
		structkey: BI.structkey,
		contentkey: BI.contentkey
	});
	
	// update status line
	if(BI.contentkey){	
		if(history.replaceState)
			history.replaceState(BI.title, null, "#"+BI.dbkey + "/" + BI.structkey);
		else location.replace("#"+BI.dbkey + "/" + BI.structkey);
		
		document.title = "CM: " + BI.dbkey + "/" + BI.title;
		var text=$L.statusbar.contentdocument;
		text += " '" + (BI.title.length>30 ? BI.title.substr(0,27) + "...":BI.title) + "' ";
		text += "<a href='#' onclick='BI.propertiespanel.show()'>";
		text += "(" + BI.contentkey + ")";
		text += "</a> " + $L.statusbar.loaded + ".";
		BI.setStatus(text);
	}
	else if(BI.structkey){
		if(history.replaceState)
			history.replaceState(BI.title, null, "#"+BI.dbkey + "/" + BI.structkey);
		else location.replace("#"+BI.dbkey + "/" + BI.structkey);

		var text=$L.statusbar.no_content_for_page;
		text += " " + BI.structkey + " " + $L.statusbar.found + ".";
		BI.setStatus(text);
	}
	else BI.setStatus($L.statusbar.no_wga_page);

	if(pageYOffset)
		iframe.window.scrollTo(0, pageYOffset);
		
	BI.sitepanel.unmask();
}

WGA.event.register(
	"portlet-website-observer",
	"CMS_title_changed", 
	function(e){
		BI.selectContent(BI.contentkey, BI.structkey);
	}
);

/*
WGA.event.register(
	"portlet-website-observer",
	"page-updated", 
	function(e){
		BI.se.updateStruct(e.params.structkey)
	}
);
*/

WGA.event.addListener("portlet-website-observer", "CMS_attachments_changed", function(){		
	var root = BI.sitepanel.iframe.window.document.body;
	var now = new Date();
	Ext.get(root).select("img.wga-urltype-intfile").each(function(el){
		var src = el.dom.src.split("?");

		var params = {}
		var cp = src[1];
		if(cp){
			cp = cp.split("=");
			for(var i=0; i<cp.length; i++){
				params[cp[0]] = cp[1]
			}
		}
		params.ts=now.getTime();
		var new_params = []
		for(var i in params)
			new_params.push(i + "=" + params[i]);
		
		el.dom.src=src[0] + "?" + new_params.join("&");
	})
})

BI.sitepanel.init=function(){

	// create mask element
	var el = Ext.get("web-content-wrapper");
	var mask = BI.sitepanel.iframemask = Ext.DomHelper.append(el.dom, {cls:"ext-el-mask"}, true);	
	mask.setDisplayed(false);

	Ext.get("web-content").on("load", BI.sitepanel.monitorIframeOnLoad);
	Ext.get("web-content").on("unload", function(){
		BI.setStatus("loading page ...");
	})

}

BI.sitepanel.mask=function(msg){
	if(BI.sitepanel.iframemask){
		var el = Ext.get("web-content-wrapper");
		BI.sitepanel.iframemask.setSize(el.dom.clientWidth, el.getHeight());
		BI.sitepanel.iframemask.update(msg||"");
		BI.sitepanel.iframemask.setDisplayed(true);
	}
}
BI.sitepanel.unmask=function(){
	if(BI.sitepanel.iframemask)
		BI.sitepanel.iframemask.setDisplayed(false);
}


BI.sitepanel.hash = location.hash
window.setInterval(function(){
	var hash = location.hash;
	if(hash!=BI.sitepanel.hash){
		BI.sitepanel.hash = hash
		try{
			var key = hash.split("/")[1];
			if(key && key!=BI.structkey){
				BI.selectContent(key);
			}
		}
		catch(e){}
	}
}, 500)


function initCustomDDHandler(iframe){

	// Ext DD Handler
	BI.sitepanel.ddhandler = new Ext.dd.DropTarget("web-content-wrapper", {
	    notifyOver: function(dragObj, e, data){
	    	var type="doc-link"
	    	if(data.grid)
	    		type = e.shiftKey ? "attachment" : "attachment-link"
	    	if(iframe.window.WGA.ddhandler.notifyOver)
	    		return iframe.window.WGA.ddhandler.notifyOver(type, dragObj.dragData) ? BI.sitepanel.ddhandler.dropAllowed : BI.sitepanel.ddhandler.dropNotAllowed
	    	else return BI.sitepanel.ddhandler.dropAllowed			    	
	    }, 
	    notifyDrop: function(dragObj, e, data){
	    	var type="doc-link"
	    	if(data.grid){
	    		type = e.shiftKey ? "attachment" : "attachment-link"
		    	if(iframe.window.WGA.ddhandler.notifyDrop){
		    		var data=[];
		    		for(var i=0; i<dragObj.dragData.selections.length; i++)
		    			data.push(dragObj.dragData.selections[i].json);
		    		return iframe.window.WGA.ddhandler.notifyDrop(type, data);
		    	}			    	
	    	}
	    	else if(iframe.window.WGA.ddhandler.notifyDrop)
	    		return iframe.window.WGA.ddhandler.notifyDrop(type, [dragObj.dragData]);			    	
	    }
	})
	BI.sitepanel.ddhandler.addToGroup("GridDD");
	BI.sitepanel.ddhandler.addToGroup("se.tree-entry");
	
	// HTML5 Drag-Drop
	iframe.document.body.addEventListener("dragenter", function(e){
		e.preventDefault();
	}, false);
	iframe.document.body.addEventListener("dragleave", function(e){
		e.preventDefault();
	}, false);

	function setDropEffect(e){
		e.dataTransfer.dropEffect = (e.shiftKey ? "link":"copy");
	}
	
	iframe.document.body.addEventListener("dragover", function(e){
		e.preventDefault();
		setDropEffect(e);
		iframe.window.WGA.ddhandler.notifyOver("HTML5-drag-over");
	}, false);
	iframe.document.body.addEventListener("drop", function(e){				
		e.preventDefault();
		var data, type;
		if(e.dataTransfer.getData("wga/files")){
			type="attachment" + (e.dataTransfer.dropEffect=="link" ? "-link" : "")
			data = JSON.parse(e.dataTransfer.getData("wga/files"))
			iframe.window.WGA.ddhandler.notifyDrop(type, data);
		}		
	}, false);

}

Ext.get("init").update("Sitepanel loaded.");
