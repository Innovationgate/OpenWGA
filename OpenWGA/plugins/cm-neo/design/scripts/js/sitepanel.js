define(["cm", "jquery"], function(CM, $){
	
	var editItemsVisible=true;
	var currentURL;

	function init(){
		$("#site-panel").on("load", function(){
			$("#loading").hide();
			initItemEditors()
			showItemEditors(true)
			if(getWindow().WGA)
				WGA.event.fireEvent("page-rendered", "sitepanel.js", getWindow().WGA.contentinfo)
		})
	}
	
	WGA.event.addListener("*", "content-changed", function(ev){
		currentURL = ev.params.href;
	})
	WGA.event.addListener("*", "CMS_cancel_item_edit", function(){
		showItemEditors(true);
	})
	WGA.event.addListener("*", "CMS_showCreateAreas", showCreateAreas)	
	
	WGA.event.addListener("*", "content-metas-changed", function(){
		reload()
	})

	WGA.event.addListener("*", "attachments-updated", function(ev){
		var _wga = getWindow().WGA
		ev.params.filename && _wga && _wga.event && _wga.event.fireEvent("file-metas-updated", "cm-neo", ev.params)
	})
	
	$(document).on("click", "a[data-wgakey]", function(ev){
		ev.preventDefault();
		reload(this.href);
	})

	function reload(url){
		try{
			var href = url || currentURL || $("#site-panel").prop("contentDocument").location;
			$("#site-panel").attr("src", href);
		}
		catch(e){}
	}
	
	function initItemEditors(){
	
		var doc = $("#site-panel").prop("contentDocument");
		$(".WGA-Item", doc).each(function(){
			var item = $(this);
	
			// ensure editors are initialized only once
			if(item.data("item-editor"))
				return;
			item.data("item-editor", true)
			
			var item_info_el=item.find(".WGA-Item-Info");
			var item_label_el=item.find(".WGA-Item-Label");
			var item_value_el=item.find(".WGA-Item-Value");
			var item_edit_el=item.find(".WGA-Item-Edit");
			
			var params=item_info_el[0].innerHTML.replace(/\s/g, "").split("|");
			
			item_edit_el.on({
				mouseover: function(){
					$(this).html("Feld bearbeiten");
				},
				mouseout: function(){
					$(this).html("");
				}
			})
			
			item.addClass("WGA-editor-"+params[1]).show();
	
			var editor_options;
			var options_el = $(".WGA-Editor-Options", item)
			if(options_el.length)
				editor_options=options_el.html().replace(/[\n\r]/g, "");

			item_edit_el.on("click", function(){
				openItemEditor.call({el:item, item:params[0], editor:params[1], options:editor_options});
			})
	
			if(params[1]=="image")
				initImgDropHandler(params[0], item)
	
		})
	}
	
	function initImgDropHandler(itemname, item){
		//console.log("initImgDropHandler", itemname, item);

		function isDesktopDrop(dt){
			return dt.types.indexOf ? (dt.types.indexOf("Files")>=0) 
				: dt.types.contains ? (dt.types.contains("Files"))
				: false;
		}
		function isFilesDrop(dt){
			return dt.types.indexOf ? (dt.types.indexOf("wga/files")>=0) 
				: dt.types.contains ? (dt.types.contains("wga/files"))
				: false;
		}

		function setDropEffect(ev){
			var dt = ev.originalEvent.dataTransfer;

			if(!isFilesDrop(dt) && !isDesktopDrop(dt))
				dt.dropEffect = "none"
			else dt.dropEffect = "copy";
		}
		
		function dragover(e) {
			setDropEffect(e);
		    e.preventDefault();
		    //console.log("img dragover")
		}
		function dragenter(e) {
			setDropEffect(e);
		    e.preventDefault();
		    //console.log("img dragenter")
		}

		function drop(ev){
			//console.log("drop", ev);
			ev.preventDefault();

			var dt = ev.originalEvent.dataTransfer;
			if(dt.getData("wga/files")){
				var data = JSON.parse(dt.getData("wga/files"))
				var file = data[0]
				//console.log("image drop", this, file.poster);
				
				var img_el = $(this).find(".WGA-Item-Value img")
				if(img_el.length){
					img_el.attr("src", file.poster);
				}
				else{
					$(this).find(".WGA-Item-Value").prepend('<img src="'+file.poster+'">')
					$(this).find(".WGA-Item-Label").css("display", "none");
				}
				storeFilenameToItem(file.name, file.container, file.dbkey, file.title || file.name);
			}

		}

		function storeFilenameToItem(filename, container, dbkey, alt){
			WGA.event.fireEvent("CMS_save_image_item", "sitepanel.js", {
				item: itemname,
				filename: filename,
				container: container,
				dbkey: dbkey,
				title: "",
				alt: alt
			});
		}

		item.on({
			"drop": drop,
			"dragenter": dragenter,
			"dragover": dragover
		})
	}
	
	function showItemEditors(show, dont_save_state){
		
		try{
			var doc = $("#site-panel").prop("contentDocument");
			if(show){
				$(".WGA-Item-Edit", doc).css("visibility", "visible").show()
				$(".WGA-Item", doc).each(function(){
					$this = $(this);
					$this.addClass("visible");
					if(!$this.find(".WGA-Item-Value").html())
						$this.find(".WGA-Item-Label").show();
				});
				editItemsVisible=true;
			}
			else {
				$(".WGA-Item-Edit", doc).css("visibility", "hidden")
				$(".WGA-Item", doc).removeClass("visible");
				editItemsVisible=false;
			}
		}
		catch(e)
		{
			console.log(e);
		}
	
	}
	
	function openItemEditor(){
		if(this.editor=="upload"||this.editor=="file"){
			CM.openDialog("upload-file")
			return;
		}
		if(this.editor=="image"){
			CM.openDialog("edit-image-item", {
				item: this.item
			})
			return;
		}
		this.el.find(".WGA-Item-Label").hide()
		showItemEditors(false, true);

		WGA.event.fireEvent("CMS_item_edit", "sitepanel.js", {
			item: this.item,
			format: this.el.find(".WGA-Item-Format").text(),
			editor: this.editor,
			options: this.options			
		});
	}


	/**
	 * show or hide create-areas of the website
	 * @param {Boolean} show
	 */
	function showCreateAreas(){

		var doc = $("#site-panel").prop("contentDocument");
		
		$(".createpage", doc).show();
		$(".BI-create", doc).css({
			position: "relative",
			zIndex: 10001
		}).show().each(function(ev){
			var info_el=$(this).find("span");
			eval("var info=" + info_el.html());
			$(this).one("click", function(ev){
				//console.log("info", info);
				CM.openDialog("create-page", info)
			})
		})
		
		var mask = $("body .body-mask", doc)
		if(!mask.length){
			$("body", doc).append('<div class="body-mask"></div>')
			mask = $("body .body-mask", doc)
		}
		mask.css({
			position: "fixed",
			opacity: .5,
			background: "black",
			top: 0,
			left: 0,
			right: 0,
			bottom: 0,
			zIndex: 10000
		}).fadeIn().on("click", function(ev){
			$(".createpage", doc).hide();
			mask.fadeOut(250, function(){
				mask.remove()
			});
		})

	}

	function getItem(item){
		var doc = $("#site-panel").prop("contentDocument");
		return $("#item_"+item, doc);
	}


	function createStyle(css, id){
		var style_id = "cm-style-"+id;
		var doc = $("#site-panel").prop("contentDocument")
		if(doc.getElementById(style_id))
			return;
	    var head = doc.getElementsByTagName("head")[0];
	    var style_el = doc.createElement("style");
	    style_el.id=style_id
	    head.appendChild(style_el);
	    
	    if(style_el.styleSheet){// IE
	        style_el.styleSheet.cssText = css;
	    }
	    else {// w3c 
	        var style_content = doc.createTextNode(css)
	        style_el.appendChild(style_content);
	    }
	}

	function removeStyle(id){
		var doc = $("#site-panel").prop("contentDocument")
		$("#cm-style-"+id, doc).remove();
	}
	
	
	function getWindow(){
		return $("#site-panel").prop("contentWindow")
	}
	
	// public interface:
	return {
		load: function(url){
			$("#site-panel").attr("src", url);
		}
		
		,iframe: function(){
			return $("#site-panel");
		}
		
		,showCreateAreas: showCreateAreas
		
		,getItem: getItem
		
		,getDocument: function(){
			return $("#site-panel").prop("contentDocument")
		}

		,getWindow: getWindow
		
		,getContentInfo: function(){
			return getWindow().WGA && getWindow().WGA.contentinfo
		}
		
		,scale: function(scale){
			return $("#site-panel").parent(".sitepanel-wrapper")
				.removeClass("scale80 scale50")
				.addClass("scale"+scale)
		}
		
		,createStyle: createStyle
		,removeStyle: removeStyle
		,showItemEditors: showItemEditors
		,initItemEditors: initItemEditors
		,isEditItemsVisible: function(){
			return editItemsVisible;
		}
						
		,reload: reload
		,init: init
	}

})
