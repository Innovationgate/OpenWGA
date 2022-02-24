// Public API
require(["cm", "uploadmanager", "sitepanel", "jquery-textarea-autogrow"], function(CM, UploadManager, Sitepanel){
	
	window.CM = {

		pageLoaded: function(info){
			WGA.event.fireEvent("page-loaded", "cm-neo", info);
		},		
		openDialog: CM.openDialog,
		initItemEditors: function(){
			Sitepanel.initItemEditors();
			if(Sitepanel.isEditItemsVisible())
				Sitepanel.showItemEditors(true)
		}
		
	}
	
	window.BI = {		// some old modules check for this
		makeTextareasResizable: function(el){
			$("#"+el+" textarea").autogrow()
		},
		UploadManager: UploadManager,
		util: {
			// dummy no-op struct-tree object
			structTree: function(id){
				id && $("#"+id).hide();
				this.tree = {
					on: function(){},
					selectPath: function(){}
				}
			}
		},
		ComboTree: function(){
			// dummy no-op combo-tree
			this.on = function(){}
			this.render = function(id){
				var el = $("#"+id);
				el.prev("input").attr("type", "text").addClass("form-control")
			}
		}
	}
		
})
