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
BI.rtf = {};

BI.rtf.TableMenu = function(config){
    BI.rtf.TableMenu.superclass.constructor.call(this, config);
    this.plain = true;
    var i = new BI.rtf.TableItem(config);
    this.add(i);
};
Ext.extend(BI.rtf.TableMenu, Ext.menu.Menu);

BI.rtf.TableItem = function(config){
    BI.rtf.TableItem.superclass.constructor.call(this, config);
};

Ext.extend(BI.rtf.TableItem, Ext.menu.BaseItem, {
   
    itemCls : "bi-table-menu",
    maskCls : 'bi-table-menu-mask',
    canActivate : true,
    showDelay: 200,
    hideDelay: 200,
  
    listAlign: 'tl-bl?',
    
    minWidth: 100,
    minHeight: 100,
    maxWidth: 300,
    maxHeight: 300,
    
    cellWidth : 14,
    cellHeight : 14,

	handleHeight: 8,
	
	restrictHeight : function() {
        var pad = this.el.getFrameWidth('tb')+this.handleHeight;
        
        var h = this.el.getHeight();
        var w = this.el.getWidth();
        
        if (w < this.minWidth) w = this.minWidth;
        if (w > this.maxWidth) w = this.maxWidth;
        if (h < this.minHeight) h = this.minHeight;
        if (h > this.maxHeight) h = this.maxHeight;
        
        w -= w % this.cellWidth;
        h -= h % this.cellWidth;

        this.el.setHeight(h+pad);
        this.el.setWidth(w+pad);
        
        this.container.setHeight(h+pad);
        this.container.setWidth(w+pad);

		this.parentMenu.el.setWidth(w+pad+8);
		this.parentMenu.el.setHeight(h+pad+8);
		
	},

    updateStatus : function(e) {
    	 var pad = this.el.getFrameWidth('tb')+this.handleHeight;
    	
    	this.tableCols = 1 + parseInt((Math.max((e.getPageX() - this.el.getX()),0)) / this.cellWidth, 10);
    	this.tableRows = 1 + parseInt((Math.max((e.getPageY() - this.el.getY()), 0)) / this.cellHeight, 10);
    
    	this.tableCols = Math.min((this.el.getWidth()-pad)/this.cellWidth, this.tableCols);
    	this.tableRows = Math.min((this.el.getHeight()-pad)/this.cellHeight, this.tableRows);
    },
    
    updateStatusInfo : function() {
    	var info = Ext.get('table_dim_info');
    	
    	if (info)
    		info.remove();
    	
    	info = this.el.createChild({
    		id: 'table_dim_info',
    		tag: 'div',
    		cls: 'info'
    	});
    	
    	info = Ext.get(info);
    	
 		info.setWidth(60);
    	info.setX(this.el.getX() + this.el.getWidth()/2 - info.getWidth()/2);
		info.setY(this.el.getY() + this.el.getHeight()/2 - info.getHeight()/2);
		
		info.insertHtml('afterBegin', '<center><b>'+this.tableCols + ' x ' + this.tableRows+'</b></center>');
    },    
    
    updateMask : function() {
    
    	var mw = this.tableCols * this.cellWidth;
    	var mh = this.tableRows * this.cellHeight;
    	
    	this.mask.setWidth(mw);
    	this.mask.setHeight(mh);
    },

    // private
    onRender : function(container, position){
        var el = document.createElement("div");        
        el.className = this.itemCls;        
        this.el = el;
        
        this.mask = Ext.get(el).createChild({tag:'div', cls:'mask'});
        
		BI.rtf.TableItem.superclass.onRender.call(this, container);
        
        Ext.get(el).on('mousemove', function(e) {
        	this.activate();
        	this.updateStatus(e);
			this.updateStatusInfo();
			this.updateMask();
			this.mask.show();
        }, this);
        
        Ext.get(this.parentMenu.el).on('mouseout', function(e) {
        	this.hideIf(e);
        }, this);
        
    	this.restrictHeight();
    	
    	this.resizer = new Ext.Resizable(this.el,  {
		    pinned:true, handles:'se'
		});
        this.resizer.on('resize', function(r, w, h){
        	this.restrictHeight();
        }, this);
    },
    
    hideIf : function(e){

        if(!e.within(this.el) && !e.within(this.container)){
        	 this.mask.hide();
    	
		     var info = Ext.get('table_dim_info');
		    	
		     if (info)
			     info.remove();
		    	
		     this.tableCols = 0;
		     this.tableRows = 0;        	
        }
    },
    
    activate : function(autoExpand){
		this.restrictHeight();
		Ext.get(document).on('mousewheel', this.hideIf, this);
    	Ext.get(document).on('mousedown', this.hideIf, this);	
    },
    
    deactivate : function() {   	
    	Ext.get(document).un('mousewheel', this.hideIf, this);
        Ext.get(document).un('mousedown', this.hideIf, this);    
    }
});


BI.rtftoolbar=function(){
	var editor;
	var dialog, toolbar, toolbarElements;
	var hideoptions, showoptions;
	var editorSelection=null;
	
	function doCommand(button, ev){
		BI.rtftoolbar.restoreEditorSelection();
		editor.execCmd(button.cmd, button.param||null);
	}

	function isCmdDisabled(cmd, ignore_focus){
		// Helperfunction: check show/hide options.
		// showoptions count more: If command is in showoptions than show the button.
		// If not in showoptions but (and) in hideoptions: hide the button. Otherwise show it.
		// The special showoption "all" defines to enabled all commands. 
		if(!ignore_focus && !editor.isEditorSelected())
			return true;
		if(showoptions.indexOf("all") != -1)
			return false;
		if (showoptions.toLowerCase().indexOf(cmd.toLowerCase()) == -1	// not in showoptions list				
				&& 
				(
					hideoptions.toLowerCase().indexOf(cmd.toLowerCase()) != -1	// not in hideoptions list
					||
					hideoptions.toLowerCase().indexOf("all") != -1
				)){
			return true;
		}
		else {
			return false;
		}
	}

	function updateButtons(toolbar) {
		var enabled;		
		toolbar.items.each(function(item){
		
			if(!editor.isEditorSelected()){
				item.disable();
			}
		
			else if(item.cmd){
				
				try{
					enabled = item.queryCommandState ? item.queryCommandState() : editor.doc.queryCommandState(item.cmd);
			   	}catch(e){		/* command not queryable */   
			   		enabled=false;
			   	}
				if(enabled)
				    item.toggle(true);
				else item.toggle(false);
			
				enabled = true;
				try{			
					if(isCmdDisabled(item.cmd) || (item.cmd_alias && isCmdDisabled(item.cmd_alias)) || (item.queryCommandEnabled && !item.queryCommandEnabled()))
						enabled=false;
				}
				catch(e){
					enabled=false;
				}
				
				if(enabled){
					if(item.disabled)
						item.enable();
				}
				else {
					if(!item.disables)
						item.disable();
				}
			}
		});
	}

	function updateMenu(menu){
		menu.items.each(function(item){
			if(item.cmd){
				enabled = true;
				try{			
					if(isCmdDisabled(item.cmd) || (item.queryCommandEnabled && !item.queryCommandEnabled(item.cmd)))
						enabled=false;
				}
				catch(e){
					enabled=false;
				}
				
				if(enabled)
					item.enable();
				else item.disable();
			}
		})
	}		

	function showLinkDialog(button, ev) {
		var opts={linktype: 'exturl'};
		var atag = editor.getNearestTagFromSelection("a");
		if(atag && atag.href){
			var linkinfo=AFW.RTF.getURLInfo(atag);
			opts={
				linktype: linkinfo.type || "exturl",
				wgakey: linkinfo.key,
				target: atag.target
			}			
		}
		//BI.rtftoolbar.saveEditorSelection();
		BI.dialog.show("rtf:insert-link", ev?ev.target:null, opts);
		BI.dialog.callback=function(url, linktext, linkinfo, target){	// linktype, target){
			editor.focus();		// Only Chrome needs this.
			BI.rtftoolbar.restoreEditorSelection();
			
			var atag = editor.createLink(url, linktext, linkinfo.type);
			if(atag){
				AFW.RTF.setURLInfo(atag, linkinfo)
				if(target)
					atag.target=target;
				else atag.removeAttribute("target")
			}
			
			BI.rtftoolbar.update();
		}
	}
	
	function showImageDialog(button, ev) {
		var opts={};
		var imgtag = editor.getNearestTagFromSelection("img");
		if(imgtag){
			var linkinfo=AFW.RTF.getURLInfo(imgtag);
			opts={
				linktype: linkinfo.type || "exturl",
				wgakey: linkinfo.key
			}
		}
		//BI.rtftoolbar.saveEditorSelection();
		BI.dialog.show("rtf:insert-image", ev?ev.target:null, opts);
		BI.dialog.callback=function(url, linktype, title, linkinfo){
			BI.rtftoolbar.restoreEditorSelection();
			var imgtag = editor.createImg(url, linktype);
			if(imgtag){
				AFW.RTF.setURLInfo(imgtag, linkinfo)
				if(title){
					imgtag.title=title;
					imgtag.alt=title;
				}
			}
			BI.rtftoolbar.update();
		}
	}

	var isEditorZoomed=false;
	var prevEditorStyle={};

	function resizeEditor(){
		this.setWidth(BI.sitepanel.iframe.el.getWidth()-20);
		this.setHeight(BI.sitepanel.iframe.el.getHeight()-85);
	}
	
	function zoomEditor(){
		var editor = BI.rtftoolbar.getEditor();
		var editorSelection = editor.getRange();
		var el = Ext.get(editor.editelement);
		prevEditorStyle = {
			y: el.getTop(),
			x: el.getLeft(),
			width: el.getWidth(),
			height: el.getHeight()		
		}
		el.setStyle({
			position: "absolute",
			overflowX: "hidden",
			overflowY: "auto",
			backgroundColor: "white",
			color: "black",
			zIndex: 10001,
			top: el.getTop(),
			left: el.getLeft(),
			width: el.getWidth(),
			height: el.getHeight()
		})
		el.shift({
			y: 70,
			x: 10,
			width: BI.sitepanel.iframe.el.getWidth()-20,
			height: BI.sitepanel.iframe.el.getHeight()-85,
			duration: .25,
			callback: function(){
				BI.sitepanel.contentMask.show();
				editor.stopResizeIframeTimer()
				var el_source = Ext.get(editor.sourcecodeElement);
				el_source.setStyle({
					position: "absolute",
					zIndex: 10001,
					top: el.getTop(),
					left: el.getLeft(),
					width: el.getWidth(),
					height: el.getHeight()
				})
				isEditorZoomed=true;
				updateButtons(toolbar)
				editor.editelement.blur();
				editor.focus();
				editor.setRange(editorSelection);
			}
		})
		Ext.EventManager.onWindowResize(resizeEditor, el);
		editor.onViewModeChange = function(mode){
			if(mode=="preview"){
				BI.sitepanel.contentMask.hide();
			}
			else {
				BI.sitepanel.contentMask.show();
				el.setY(70);
			}
		}
	}
	function unZoomEditor(){

		var editor = BI.rtftoolbar.getEditor();
		var editorSelection = editor.getRange();
		var el = Ext.get(editor.editelement);
		Ext.EventManager.removeResizeListener(resizeEditor, el);
		BI.sitepanel.contentMask.hide();
		editor.onViewModeChange = null;
		el.shift({
			y: prevEditorStyle.y,
			x: prevEditorStyle.x,
			width: prevEditorStyle.width,
			height: prevEditorStyle.height,
			duration: .25,
			callback: function(){
				el.setStyle({
					position: "",
					overflow: "",
					backgroundColor: "",
					color: "",
					zIndex: "",
					top: "",
					left: "",
					width: "",
					height: ""
				})
				var el_source = Ext.get(editor.sourcecodeElement);
				el_source.setStyle({
					position: "",
					zIndex: "",
					top: "",
					left: "",
					width: "",
					height: ""
				})

				isEditorZoomed=false;
				editor.restartResizeIframeTimer();
				updateButtons(toolbar);
				editor.editelement.blur();
				editor.focus();
				editor.setRange(editorSelection);
			}
		});
	}


	// context menu:
	var statusbar_context=new Ext.menu.Menu({
		items: [
        	{	text: "Edit Node", 
        		handler: function(m, ev){
        			BI.dialog.show("rtf:edit-rtf-node", null, statusbar_context.params)
        		}
        	}
        	,{	text: "Remove Node", 
        		handler: function(m, ev){
					var node = BI.rtftoolbar.selectionPath[statusbar_context.params.nodeindex];
					editor.selection.save();
					editor.removeNode(node);
					editor.selection.restore();
					BI.rtftoolbar.selection.selectedIndex = -1;
					BI.rtftoolbar.update();
        		}
        	}
        	,{	text: "Remove Styles", 
        		handler: function(m, ev){
        			var node = BI.rtftoolbar.selectionPath[statusbar_context.params.nodeindex];
					var t = node.innerHTML;
					if(t){
						t = t.replace(/style="[^"]*"/g, "");		// remove all styles
						t = t.replace(/class="([^"]*)"/g, function(str, cls){
							if(cls.indexOf("wga-")>=0)
								return "class=\""+cls+"\""
							return "";
						});
						node.innerHTML = t;
					}
					node.removeAttribute("style");
					node.removeAttribute("class");
					BI.rtftoolbar.selection.selectedIndex = -1;
					BI.rtftoolbar.update();
					editor.setRange();
        		}
        	}
		]
	});
	
	statusbar_context.on("beforehide", function(){
		Ext.get("web-content-wrapper").unmask();
		BI.rtftoolbar.selection.selectedIndex = -1;
	});

	function showContentMenu(ev, i, tag){

		BI.rtftoolbar.selection.selectedIndex = i;
		
		Ext.get("web-content-wrapper").mask();

		var r = editor.getRange();
		r.selectNode(BI.rtftoolbar.selectionPath[i]);
		editor.focus();
		editor.setRange(r);

		ev = Ext.EventObject.setEvent(ev)
		ev.getTarget("a", 10, true).addClass("selected");
		
		var xy=ev.xy;
		Ext.lib.Event.stopEvent(ev);
		var h=statusbar_context.getEl().getHeight();
		var doc_h=Ext.lib.Dom.getViewportHeight();
		if(xy[1]+h>doc_h)
			xy[1]-=h;
		statusbar_context.showAt(xy);
		statusbar_context.params={
			nodeindex: i,
			tag: tag 
		}
	}

			
	return{
		/*
			public interface
		*/
		/*
		isEditorZoomed: function(){return isEditorZoomed},
		zoomEditor: zoomEditor,
		unZoomEditor: unZoomEditor,
		*/

		showLinkDialog: showLinkDialog,
		showImageDialog: showImageDialog,
		updateButtons: updateButtons,
		updateMenu: updateMenu,
		isCmdDisabled: isCmdDisabled,
		
		setEditor: function(e){
			editor = e;
			if(e)
				editor.toolbar = BI.rtftoolbar;
		},
		getEditor: function(){
			return editor;
		},
				
		getShowOptions: function() {
			return showoptions;
		},

		getHideOptions: function() {
			return hideoptions;
		},
				
		update: function(eventType){
			if(!editor)
				return;
			BI.rtftoolbar.saveEditorSelection();
			updateButtons(toolbar);
			if(BI.rtftoolbar.toolpanel)
				BI.rtftoolbar.toolpanel.update(eventType);
			
			// show path on console:
			if(editor.viewmode=="wysiwyg" && editor.isEditorSelected() && editor.getPathFromSelection){
				var path = editor.getPathFromSelection();
				BI.rtftoolbar.selectionPath = [];
				var txt = "";
				var i = 0;			//path.length;
				for(var i=0; path.length; i++){
					var node = path.pop(); 
					if(!node.tagName)
						continue;
					BI.rtftoolbar.selectionPath.push(node);
					
					if(WGA.isIE){
						txt += " &lt;" + node.tagName + "&gt;";						
						continue;
					}
					
					var scr_select = "BI.rtftoolbar.selection.selectNode("+i+")";
					var scr_edit = "BI.rtftoolbar.selection.editNode(event, " + i + ",\"" + node.tagName + "\")";
					var className = node.className.replace(/wga-\S+/g, "").replace(/^\s+|\s+$/g, "")	// trim
					var stl = (node.getAttribute("style") || className) ? "style='color:red'" : "";
					var cls = (BI.rtftoolbar.selection.selectedIndex==i ? "class='selected'" : "");
					txt += "<a " + cls + stl + " href='#' onclick='"+scr_select+"'"
					if(!isCmdDisabled("html"))
						txt += " oncontextmenu='"+scr_edit+"'"
					txt += ">" + node.tagName;
					if(className)
						txt += "."+className
					txt += "</a>";
				}
				var s = editor.getSelectedText() || "";	//.toString();
				if(s.length > 40)
					s = (s.substr(0, 20) + " ... " + s.substr(s.length-15, 15));
				if(s)
					txt += " <span style='background-color:beige;padding: 0 2px'>" + s + "</span>"; 
				BI.setStatus("<span class='rtf-path'>RTF Selection: " + txt + "</span>");
			}
			
		},
	

		selection: {
			selectedIndex: -1
		
			,selectNode: function(i){
				var r = editor.getRange();
				r.selectNode(BI.rtftoolbar.selectionPath[i]);
				editor.focus();
				editor.setRange(r);
			}
			,editNode: function(ev, i, tag){
				showContentMenu(ev, i, tag);
			}
		},

		show: function(){
			dialog.show();
			editor.focus();
			BI.rtftoolbar.update.defer(250, this, []);	// defere this so that RTF-editor is displayed quickly
		},
		
		hide: function(){
			dialog.hide();
		},

		showToolbarDialog: function(){
			dialog.show();
		}, 

		saveEditorSelection: function(){
			if(WGA.isIE)
				editorSelection = editor.getRange();
			else editor.selection.save();
		},
		restoreEditorSelection: function(){
			if(WGA.isIE){
				if(editorSelection){
					editor.setRange(editorSelection);				
				}
				editor.focus();
			}
			else {
				editor.selection.restore();
			}
		},

		init: function(hideArray, showArray) {
			var defaultHideOptions = "ForeColor, BackColor, FontName, FontSize, editHTML";
			var defaultShowOptions = "zoom, edit-helper,Undo,Redo,RemoveFormat";
			if(!WGA.isIE)
				defaultHideOptions += ",Paste";
			hideoptions=hideArray + "," + defaultHideOptions;
			showoptions=showArray + "," + defaultShowOptions;
			// update position:
			var iframe=Ext.get("web-content-wrapper");
			dialog.moveTo(iframe.getX()+5, iframe.getTop()+5);
			isEditorZoomed=false;
		},
		
		editorLostFocus: function(){
			//console.log("lost focus");
			BI.rtftoolbar.update.defer(250, this, []);
		},
		editorGotFocus: function(){
			//console.log("got focus");
			BI.rtftoolbar.update.defer(250, this, []);
		},		
		
		doKeyFunction: function(ev){
			var key = String.fromCharCode(ev.keyCode || ev.which).toLowerCase();			
			switch(key){
				case "b":
					editor.execCmd("Bold");
					return true;
				case "i":
					editor.execCmd("Italic");
					return true;
				case "u":
					editor.execCmd("Underline");
					return true;
				case "p":
					editor.execCmd("FormatBlock", "p");
					return true;
				/*case "p":					
					this.showImageDialog();
					return true;*/
				/*case "l":
					this.showLinkDialog();
					return true;*/
				case "s":
					BI.contenteditor.saveFieldAndContinue();
					return true;
				case "z":
					isEditorZoomed ? unZoomEditor() : zoomEditor();
					return true;
			}
			return false;
		},
		
		createToolbar: function(el){

			dialog = new Ext.BasicDialog("web-content-rtf-toolbar", { 
				//autoCreate: true,
				autoScroll: false,
		        modal:false,
				resizable: false,
				collapsible: true,
				closable: false,
				shim: true,
		        shadow:true,
		        title: "Content Manager Richtext Toolbar"
			});
			
			dialog.on('show', function() {	
				// resize dialog height on show, to make toolbar fit in dialog with any theme
				var tb_el = this.getEl().child(".x-toolbar");
				var toolbarHeight = tb_el.getHeight();
				var toolbarWidth = tb_el.child("table").getWidth() + tb_el.getPadding("lr");
				this.setContentSize(toolbarWidth, toolbarHeight);
				
				if(WGA.isWebKit){
					editor.focus();
					editor.doc.defaultView.getSelection().collapseToStart();
				}
				
			}, dialog);

			var dd=dialog.dd;
			dd.startDrag = function(){
				BI.sitepanel.mask();	
				dialog.startMove.createDelegate(dialog);
			}
			dd.endDrag = function(){ 
				BI.sitepanel.unmask();
				dialog.endMove.createDelegate(dialog);
			}

			var webPanel = BI.layout.getWebPanel();			
			webPanel.on("activate", function(){
				Ext.get("web-content-rtf-toolbar-wrapper").setStyle("display", "block");
			}, this)
			webPanel.on("deactivate", function(){
				Ext.get("web-content-rtf-toolbar-wrapper").setStyle("display", "none");
			}, this);
			
			var tableMenu = new BI.rtf.TableMenu({
				shadow: false,
				handler: function() {
					BI.rtftoolbar.restoreEditorSelection();
					editor.createTable(this.tableRows, this.tableCols, '100%', '');
				}
			});
			tableMenu.on('show', function(){
				//BI.rtftoolbar.saveEditorSelection();
				BI.sitepanel.mask();				
			});
			tableMenu.on('hide', function(){
				BI.sitepanel.unmask();	
			});

			toolbarElements = [
				{
					cmd: "zoom",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/shape_move_forwards.png",
					tooltip: $L.RTFToolbar.zoom,
					handler: function(b){
						if(isEditorZoomed)
							unZoomEditor();
						else zoomEditor();
					},
					queryCommandState: function(){
						return isEditorZoomed;
					}
				},
				{
					cmd: "edit-helper",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/tag.png",
					tooltip: $L.RTFToolbar.editHelper,
					handler: function(){
						editor.toggleEditHelper();
						BI.rtftoolbar.editHelper = editor.hasEditHelper();
						BI.rtftoolbar.restoreEditorSelection();
					},
					queryCommandState: function(){
						return editor.hasEditHelper();
					}
				},
				'-',
				
				/*{
					cmd: "Undo",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/arrow_undo.png",
					tooltip: $L.RTFToolbar.undo,
					handler: doCommand
				},
				{
					cmd: "Redo",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/arrow_redo.png",
					tooltip: $L.RTFToolbar.redo,
					handler: doCommand
				},
				'-',*/
				{
					cmd: "RemoveFormat",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/html_valid.png",
					tooltip: $L.RTFToolbar.removeFormatting,
					handler: function(button, ev){
						if(editor.getSelectedText())
							editor.selection.clean();
						else editor.cleanHTML()						
					}
				},				
				'-',
				{
					cmd: "Bold",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_bold.png",
					tooltip: $L.RTFToolbar.bold,
					handler: doCommand
				},
				{
					cmd: "Italic",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_italic.png",
					tooltip: $L.RTFToolbar.italic,
					handler: doCommand
				},
				{
					cmd: "Underline",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_underline.png",
					tooltip: $L.RTFToolbar.underline,
					handler: doCommand
				},
				"-",
				{
					cmd: "Sub",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_subscript.png",
					tooltip: $L.RTFToolbar.subscript,
					queryCommandState: function(){
						var el = editor.getNearestTagFromSelection("sub"); 
						return el!=null
					},
					handler: doCommand
				},
				{
					cmd: "Sup",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_superscript.png",
					tooltip: $L.RTFToolbar.superscript,
					queryCommandState: function(){
						var el = editor.getNearestTagFromSelection("sup"); 
						return el!=null
					},
					handler: doCommand
				},
				"-",
				{
					cmd: "JustifyLeft",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_align_left.png",
					tooltip: $L.RTFToolbar.justifyLeft,
					handler: doCommand
				},
				{
					cmd: "JustifyCenter",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_align_center.png",
					tooltip: $L.RTFToolbar.justifyCenter,
					handler: doCommand
				},
				{
					cmd: "JustifyRight",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/text_align_right.png",
					tooltip: $L.RTFToolbar.justifyRight,
					handler: doCommand
				},
				{
					cmd: "JustifyFull",
					cls: "x-btn-icon",
					tooltip: $L.RTFToolbar.justifyFull,
					icon: "../../plugin-wga-app-framework/file/icons/text_align_justify.png",
					handler: doCommand
				},
				"-",
				{
					cmd: "Indent",
					cls: "x-btn-icon",
					tooltip: $L.RTFToolbar.indent,
					icon: "../../plugin-wga-app-framework/file/icons/text_indent.png",
					handler: doCommand
				},
				{
					cmd: "Outdent",
					cls: "x-btn-icon",
					tooltip: $L.RTFToolbar.outdent,
					icon: "../../plugin-wga-app-framework/file/icons/text_indent_remove.png",
					handler: doCommand
				},
				"-",
				{
					cmd: "InsertUnorderedList",
					cls: "x-btn-icon",
					tooltip: $L.RTFToolbar.bulletList,
					icon: "../../plugin-wga-app-framework/file/icons/text_list_bullets.png",
					handler: doCommand
				},
				{
					cmd: "InsertOrderedList",
					cls: "x-btn-icon",
					tooltip: $L.RTFToolbar.enumList,
					icon: "../../plugin-wga-app-framework/file/icons/text_list_numbers.png",
					handler: doCommand
				},
				"-",
				{
					cmd: "InsertImg",
					//cmd_alias: "InsertSimpleImg",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/pictures.png",
					tooltip: $L.RTFToolbar.insertImage,
					handler: function(m, ev){
						showImageDialog(m, ev);
					}				
				},
				/*{
					cmd: "EditImg",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/picture_edit.png",
					tooltip: $L.RTFToolbar.editImage,
					queryCommandEnabled: function(){
						var img = editor.getNearestTagFromSelection("img");
						if(!img)
							return false;
						var urlinfo = AFW.RTF.getURLInfo(img)
						return (urlinfo.type == "intfile");
					},
					handler: function(m, ev){
						var img = editor.getNearestTagFromSelection("img");
						if(img){
							var urlinfo = AFW.RTF.getURLInfo(img)
							BI.resizableDialog.show("view-image", m.el, {filename:urlinfo.key});
						}
					}				
				},*/
				{
					cmd: "InsertTable",
					cmd_alias: "TableProperties",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/table_add.png",
					tooltip: $L.RTFToolbar.insertTable,
					menu: tableMenu
				},
				{
					cmd: "InsertLink",
					//cmd_alias: "InsertSimpleLink",
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/link.png",
					tooltip: $L.RTFToolbar.insertLink,
					handler: showLinkDialog	
				},
				{
					cls: "x-btn-icon",
					icon: "../../plugin-wga-app-framework/file/icons/link_delete.png",
					tooltip: $L.RTFToolbar.unlink,
					cmd: "Unlink",
					queryCommandEnabled: function(){
						var link = editor.getNearestTagFromSelection("a"); 
						return link && link.href;
					},
					handler: doCommand	
				}
			];

			if(Ext.isIE){
				toolbarElements.unshift(
					{
						cmd: "Paste",
						cls: "x-btn-icon",
						icon: "../../plugin-wga-app-framework/file/icons/paste_plain.png",
						tooltip: $L.RTFToolbar.paste,
						handler: doCommand
					},
					'-'
				)
			}
			/*
			else {
				toolbarElements.unshift(
					{
						cmd: "Undo",
						cls: "x-btn-icon",
						icon: "../../plugin-wga-app-framework/file/icons/arrow_undo.png",
						tooltip: $L.RTFToolbar.undo,
						handler: doCommand
					},
					{
						cmd: "Redo",
						cls: "x-btn-icon",
						icon: "../../plugin-wga-app-framework/file/icons/arrow_redo.png",
						tooltip: $L.RTFToolbar.redo,
						handler: doCommand
					},
					'-'
				)
			}
			*/

			toolbar = new Ext.Toolbar(dialog.body, toolbarElements);			
		}
	
	}	
}();

