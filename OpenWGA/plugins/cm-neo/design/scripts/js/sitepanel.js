define(["cm", "jquery"], function(CM, $){
	
	var editItemsVisible=true;
	var currentURL;
	
	WGA.event.addListener("*", "content-changed", init)
	WGA.event.addListener("*", "CMS_cancel_item_edit", function(){
		showItemEditors(true);
	})
	WGA.event.addListener("*", "CMS_showCreateAreas", showCreateAreas)	
	
	WGA.event.addListener("*", "content-metas-changed", function(){
		reload()
	})

	WGA.event.addListener("*", "attachments-updated", function(ev){
		var _wga = getWindow().WGA
		ev.params.filename && _wga && _wga.event.fireEvent("file-metas-updated", "cm-neo", ev.params)
	})
	
	function init(ev){
		currentURL = ev.params.href;
		initItemEditors()
		showItemEditors(true)
	}

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
			
			item.addClass(params[1]).show();
	
			var editor_options;
			var options_el = $(".WGA-Editor-Options", item)
			if(options_el.length)
				editor_options=options_el.html().replace(/[\n\r]/g, "");

			item_edit_el.on("click", function(){
				openItemEditor.call({el:item, item:params[0], editor:params[1], options:editor_options});
			})
	
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
			}
			else {
				$(".WGA-Item-Edit", doc).css("visibility", "hidden")
				$(".WGA-Item", doc).removeClass("visible");
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
			return getWindow().WGA.contentinfo
		}
		
		,scale: function(scale){
			return $("#site-panel").parent(".sitepanel-wrapper")
				.removeClass("scale80 scale50")
				.addClass("scale"+scale)
		}
		
		,createStyle: createStyle
		,removeStyle: removeStyle
		,showItemEditors: showItemEditors
		
		,reload: reload
	}

})
