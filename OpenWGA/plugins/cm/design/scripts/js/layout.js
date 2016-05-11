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
 * create the panel layout for the application
 */

Ext.get("init").update("create panels ...");

BI.layout = function(){
	// locals:
	var layout;
	var webPanel, sePanel;
	var hasLayout=false;
	
    return {
		init : function(){
			// create the main layout
            layout = new Ext.BorderLayout(document.body, {
                hideOnLayout: true,
                west: {
                    split:true,
					autoHide:true,
                    initialSize: 225,
                    minSize: 200,
                    maxSize: 400,
                    autoScroll:false,
                    collapsible:true,
                    collapsed:true,
                    titlebar: true,
                    title: "Explorer",
                    tabPosition:"top",
                    useShim:true,
                    showPin: true,
                    animate: BI.animate.panels,
                    cmargins: {top:2,bottom:0,right:0,left:0}
                },
                center: {
                    //autoScroll:true,
                    useShim:true,
                    tabPosition:"top",
                    closeOnTab: true,
                    titlebar:false
                },
                north: {
					initialSize: 29,
					border: 0
                },
                south: {
                    initialSize: 22
                }
            });

            layout.beginUpdate();
			
			sePanel = new Ext.ContentPanel('siteexplorer-panel', {
            		title: 		$L.layout.sitesstructure, 
            		fitToFrame:	true, 
            		toolbar: 	BI.se.createToolbar("siteexplorer-tb"), 
            		resizeEl:	'siteexplorer-body'
            })
            layout.add('west', sePanel);

			/*
			var childPanel = new Ext.ContentPanel('childexplorer-panel', {
            		title: 		"children", 
            		fitToFrame:	true ,
            		//toolbar: 	BI.se.createToolbar("childexplorer-tb"), 
            		resizeEl:	'childexplorer-body'
            })
            layout.add('west', childPanel);
            */

			layout.add('north', new Ext.ContentPanel('header-panel'));
			layout.add('south', new Ext.ContentPanel('status-panel'));
			// create nested layout for site-panel:
			BI.sitepanel.layout.init();			
			webPanel = layout.add('center', new Ext.NestedLayoutPanel(BI.sitepanel.layout.getLayout(), 'Website ' + BI.dbtitle));			            
			webPanel.on("activate", function(){
				if(BI.contenteditor)
					BI.showToolbarButtons("fieldedit");
				else BI.showToolbarButtons("cms");
				/*
				var p = this.findPanel('siteexplorer-panel');
				if(p)
					p.region.showPanel(p);
				*/
			}, this);
			
			layout.endUpdate();
			layout.restoreState();
			hasLayout=true;			
		},
		
		getWebPanel: function(){
			return webPanel;
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
			layout.getRegion("west").showPanel("siteexplorer-panel");
			layout.getRegion("center").showPanel(webPanel);
		},
		
		getRegion: function(reg){
			return layout.getRegion(reg);
		},
		
		hasLayout: function(){
			return hasLayout;
		}
	}				
}();

Ext.get("init").update("Panels created.");
