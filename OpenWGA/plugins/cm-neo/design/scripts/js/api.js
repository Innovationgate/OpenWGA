// Public API
require(["cm", "uploadmanager", "jquery-textarea-autogrow"], function(CM, UploadManager){
	
	window.CM = {

		pageLoaded: function(){
			$("#site-panel").trigger("load.sitepanel")
		},
		
		openDialog: CM.openDialog

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
