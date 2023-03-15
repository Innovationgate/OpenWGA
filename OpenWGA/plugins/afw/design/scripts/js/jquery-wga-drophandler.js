/*
 *	jquery-plugin wga-drophandler
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	$.fn.wga_drophandler = function(config){
	
		function isDesktopDrop(dt){
			return dt.types.indexOf ? dt.types.indexOf("Files")>=0 
					: dt.types.contains ? dt.types.contains("Files")
					: false
		}
		function isFilesDrop(dt){
			return dt.types.indexOf ? dt.types.indexOf("wga/files")>=0 
					: dt.types.contains ? dt.types.contains("wga/files")
					: false
		}
		function isLinkDrop(dt){
			return dt.types.indexOf ? dt.types.indexOf("wga/link")>=0 
					: dt.types.contains ? dt.types.contains("wga/link")
					: false
		}
		
		return this.each(function(){
			var types = config.types||"";
			var ops = config.ops || ["link","copy"];
			
			var onDesktopDrop = config.onDesktopDrop;
			var onFilesDrop = config.onFilesDrop || (types.indexOf("files")>=0 && config.handler);
			var onLinkDrop = config.onLinkDrop;

			$(this).on({

				"dragenter": function(e){
					var dt = e.originalEvent.dataTransfer;
					if(isFilesDrop(dt) || isDesktopDrop(dt) || isLinkDrop(dt))					
						$(this).addClass("dragover");
					return false;
				},

				"dragleave": function(e){
					$(this).removeClass("dragover");
					return false;
				},

				"dragover": function(e){
					var dt = e.originalEvent.dataTransfer;

					if(isLinkDrop(dt))
						dt.dropEffect = "link";				
					else if(!isFilesDrop(dt) && !isDesktopDrop(dt) && !isLinkDrop(dt))
						dt.dropEffect = "none"
					else if(e.shiftKey)
						dt.dropEffect = ops.indexOf("link")>=0 ? "link" : "none"
					else dt.dropEffect = ops.indexOf("copy")>=0 ? "copy" : "none"
					
					return false;
				},

				"drop": function(e){
					var dt = e.originalEvent.dataTransfer; 
					$(this).removeClass("dragover");
					if(isDesktopDrop(dt)){
						if(dt.files && dt.files.length && onDesktopDrop)
							onDesktopDrop.call(this, dt.files, e.shiftKey);
						return false;
					}
					try{
						if(isFilesDrop(dt)){
							var data = JSON.parse(dt.getData("wga/files")||[])
							//console.log("afw files drop", onFilesDrop, data)
							if(data && data.length && onFilesDrop){
								onFilesDrop.call(this, data, e.shiftKey)
								return false;
							}
						}
						if(isLinkDrop(dt)){
							var data = JSON.parse(dt.getData("wga/link"))
							//console.log("afw link drop", onLinkDrop, data)
							if(data && onLinkDrop){
								onLinkDrop.call(this, data, e.shiftKey)
								return false;
							}
						}						
					}
					catch(e){
						// unable to parse JSON?
						//console.log(e)
					}
					
					return false;
				}
			})
		})
	}
	
})
