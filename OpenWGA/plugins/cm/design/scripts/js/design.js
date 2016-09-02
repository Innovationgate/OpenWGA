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
BI.design={};

BI.design.createToolbar=function(el){
	// create-design menu:
	BI.menu.designCreateToolbarMenu=new Ext.menu.Menu({
		items:[
        	{	text: $L.toolbarmenu.newContentType,
        		icon: "../../plugin-wga-app-framework/file/icons/page_add.png",
        		handler: function(){
		    		BI.design.contenttype_panel.show({}, "new");        			
        		}
        	},
			{	text: $L.toolbarmenu.newArea,
        		icon: "../../plugin-wga-app-framework/file/icons/page_add.png",
        		handler: function(){
		    		BI.design.areapanel.show({}, "new");        			
        		}
        	},
			{	text: $L.toolbarmenu.newLanguage,
        		icon: "../../plugin-wga-app-framework/file/icons/page_add.png",
        		handler: function(){
		    		BI.design.languagepanel.show({}, "new");
        		}
        	},
			{	text: $L.toolbarmenu.newFileContainer,
        		icon: "../../plugin-wga-app-framework/file/icons/page_add.png",
        		observer: "mayEditFileContainer",
        		handler: function(){
		    		BI.design.filepanel.show({}, "new");        			
        		}
        	},
			{	text: $L.toolbarmenu.newWorkflow,
				observer: "mayCreateWorkflows",
        		icon: "../../plugin-wga-app-framework/file/icons/page_add.png",
        		handler: function(){
		    		BI.design.workflowpanel.show({}, "new");        			
        		}
        	}
				
		]
	});

	BI.toolbarbuttons.design=[
		new Ext.Toolbar.Button({
				cls: "x-btn-text-icon",
				icon: "../../plugin-wga-app-framework/file/icons/application_double.png",
				text: "Erstellen",
				menu: BI.menu.designCreateToolbarMenu
		})
	];
	for(var i=0; i<BI.toolbarbuttons.design.length; i++){
		BI.toolbar.add(BI.toolbarbuttons.design[i])
	}
		
	var tb = BI.menu.designExplorerToolbar = new Ext.Toolbar(el);
    tb.add(
		{	cls: "x-btn-icon",
			icon: "../../plugin-wga-app-framework/file/icons/page_refresh.png",
			handler: function(){BI.design.reload()},
			tooltip: $L.designExplorer.refreshTooltip
		},
		{
			cls: "x-btn-icon",
			icon: "../../plugin-wga-app-framework/file/icons/page_white_add.png",
			menu: BI.menu.designCreateToolbarMenu 
    	}
		
	);
	return tb;
}
	
BI.design.createExplorer=function(){
	if(!BI.layout.findPanel('designexplorer-panel')){
		var panel = new Ext.ContentPanel('designexplorer-panel', {
        		title: 		$L.designExplorer.designTabTitle,
        		fitToFrame:	true, 
        		toolbar: 	BI.design.createToolbar("designexplorer-tb"), 
        		resizeEl:	'designexplorer-body'
       	})
		Ext.get("designexplorer-body").load("./system:portlet-includer.int.html", {
			dbkey: BI.dbkey,
			portlet_tml: "design:explorer:portlet-explorer"
		});
		
		BI.layout.addPanel('west', panel);
       	BI.layout.getRegion('west').showPanel('siteexplorer-panel');
	}
}
BI.design.showExplorer=function(){
	if(BI.layout.findPanel('designexplorer-panel'))
		BI.layout.getRegion('west').showPanel('designexplorer-panel');
}
BI.design.removeExplorer=function(){
	if(BI.layout.findPanel('designexplorer-panel')){
		BI.layout.getRegion('west').remove('designexplorer-panel', true);
		BI.design.contenttype_panel.destroy();
		BI.design.workflowpanel.destroy();
		BI.design.filepanel.destroy();
		BI.design.languagepanel.destroy();
		BI.design.areapanel.destroy();
	}
}

BI.design.designTree=function(el, panel, type, title){

	var Tree = Ext.tree;
    this.panel=panel;
 	panel.on("removed", function(params){ 
		this.unselect()
	}, this)
	
    this.selectedID=null;
	
	this.tree = new Tree.TreePanel(el, {
        animate:true, 
        loader: new Tree.TreeLoader({
        	dataUrl:'./design:explorer:get-design-nodes.int.html',
        	baseParams: {
        		dbkey: BI.dbkey
        	}
        }),
        enableDrag:true,
        ddGroup: "design-"+type,
        rootVisible:true,
        lines: false,
        singleExpand: false
    });
	new Ext.tree.TreeSorter(this.tree);
	
	this.tree.on("beforeclick", function(node, ev){
		if(node==root){
			root.toggle();
			return false;
		}
		this.panel.mode="view";
		return true;
	}, this)

	this.clickdelay=new Ext.util.DelayedTask;
	this.tree.on("click", function(node, ev){
		//console.log("click", ev);
		this.clickdelay.delay(250, function(){
			if(node.leaf){
				this.panel.show({
					id:node.id
				});
				this.selectedID=node.id;
			}
			else {
				node.toggle();
				this.selectedID=null;
			}
		}, this);
	}, this);
	
	this.tree.on("dblclick", function(node, ev){				
		//console.log("dblclick", ev);				
		this.clickdelay.cancel();
		if(node.leaf)
			this.panel.show({
				id:node.id
			}, "edit");
		else node.expand();
	}, this);

	
    var root = new Tree.AsyncTreeNode({
        text: title||type,		//BI.dbkey,
        icon: "../../plugin-wga-app-framework/file/icons/database.png",
        //cls: "root", 
        cls: "light-panel-bg tree-root",
        //cls: "x-layout-panel-hd tree-root",
        draggable:false, // disable root node dragging
        expanded: false,
        id:type
    });
    
    this.tree.setRootNode(root);
    this.tree.render();
    //root.expand(false, true);	   
    
    this.reload=function(callback){    	
    	root.reload(callback)
    } 
    
    this.removeSelectedNode=function(){
    	if(this.selectedNode){
    		this.selectedNode.parentNode.removeChild(this.selectedNode);
    		this.selectedNode=null;
    	}
    }
    
    this.selectPath=function(path){
    	//console.log("tree.select path", path)
		this.tree.selectPath(path, undefined, function(success, node){
			//console.log("selectPath callback");
			if(!success){
				//console.log("path " + path + " nicht gefunden.", this)
				this.reload(function(){
					this.tree.selectPath(path, null, function(success, node){
						if(success)
							this.selectedNode=node;
					}.createDelegate(this));
				}.createDelegate(this));
			}
			else this.selectedNode=node;
		}.createDelegate(this))
    }	
    
    this.unselect=function(){
    	if(this.selectedNode){
    		try{
    			this.selectedNode.unselect();
    		}catch(e){
    			WGA.util.showException("unable to unselect node", e);
    		}
    	}
    }
}


BI.design.panel=function(el, title, tml, toolbar){
	
	var tb = [
		{
			cls: "x-btn-text",
			hidewhen: "new,view,edit",
			text: $L.deleting
		},
				
		{
			cls: "x-btn-text-icon",
			icon: "../../plugin-wga-app-framework/file/icons/page_save.png",
			text: $L.save,
			hidewhen: "view,delete",
			scope: this,
			handler: function(m, ev){
				this.submit(false);	// implemented in included TML-module
			}
		},
		{
			cls: "x-btn-text-icon",
			icon: "../../plugin-wga-app-framework/file/icons/page_save.png",
			hidewhen: "view,new,delete",
			text: $L.saveandclose,
			scope: this,
			handler: function(m, ev){
				this.submit(true);	// implemented in included TML-module
			}
		},
		{
			cls: "x-btn-text-icon",
			icon: "../../plugin-wga-app-framework/file/icons/page_go.png",
			hidewhen: "view,new,delete",
			text: $L.cancel_edit,
			scope: this,
			handler: function(){
				this.mode="view";
				this.reload();
			}
		},
		{
			cls: "x-btn-text-icon",
			icon: "../../plugin-wga-app-framework/file/icons/page_edit.png",
			hidewhen: "edit,new,delete",
			text: $L.edit,
			id: "edit",
			scope: this,
			handler: function(){
				this.reload("edit");
			}
		},
		{
			cls: "x-btn-text-icon",
			icon: "../../plugin-wga-app-framework/file/icons/page_delete.png",
			hidewhen: "new,delete",
			id: "delete",
			text: $L.remove,
			scope: this,
			handler: function(){
				this.reload("delete");
			}
		}
	]

    BI.design.panel.superclass.constructor.call(this, el, title, "system:portlet-includer.int.html", tb);
    this.menuToolbarButtons="design";
	this.defaultParams={portlet_tml: tml};
	
	this.on("activate", function() {
		BI.design.showExplorer();
	});
	
	this.on("refresh", function(mode){
		//console.log("refresh", mode)
		this.toolbar.items.each(function(button){
			if(button.hidewhen && button.hidewhen.indexOf(mode)>=0)
				button.hide();
			else button.show();
		})
	})
	
	this.disableToolbarButtons = function(mode){
		this.toolbar.items.each(function(item){
			if(item.hidewhen && item.hidewhen.indexOf(mode)>=0)
				item.disable();
			else item.enable()
		})
	};
	
	this.getId=function(){
		return this.params.id;
	}
	
}
Ext.extend(BI.design.panel, BI.panel);

// Contenttype panel
BI.design.contenttype_panel=new BI.design.panel("design-contenttype-panel", $L.panels.contenttype, "design:portlet-contenttype");
BI.design.contenttype_panel.updateTitle=function(params){
	this.setTitle($L.panels.contenttype + ": " + (params.id||" " + $L.panels.alternativeParam))
}

// Area panel
BI.design.areapanel=new BI.design.panel("design-area-panel", $L.panels.area, "design:portlet-area");
BI.design.areapanel.updateTitle=function(params){
	this.setTitle($L.panels.area + ": " + (params.id||" " + $L.panels.alternativeParam))
}

// Language panel
BI.design.languagepanel=new BI.design.panel("design-language-panel", $L.panels.language, "design:portlet-language");
BI.design.languagepanel.updateTitle=function(params){
	this.setTitle($L.panels.language + ": " + (params.id||" " + $L.panels.alternativeParam))
}

// File container panel
BI.design.filepanel=new BI.design.panel("design-file-panel", $L.panels.filecontainer, "design:portlet-filecontainer");
BI.design.filepanel.updateTitle=function(params){
	this.setTitle($L.panels.filecontainer + ": " + (params.id||" " + $L.panels.alternativeParam))
}

// Workflow panel
BI.design.workflowpanel=new BI.design.panel("design-workflow-panel", $L.panels.workflow, "design:portlet-workflow");
BI.design.workflowpanel.updateTitle=function(params){
	this.setTitle($L.panels.workflow + ": " + (params.id||" " + $L.panels.alternativeParam))
}
