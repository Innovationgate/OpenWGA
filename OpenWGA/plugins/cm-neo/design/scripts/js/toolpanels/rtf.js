define(["jquery", "cm", "multi-select", "afw/rtfeditor"], function($, CM, MS){

	var editor;
	var options;
	var selectedImg;

	function setClasses(el, classes){
		editor.focus();
		if(el){
			for(let cls in classes){
				if(classes[cls])
					$(el).addClass(cls)
				else $(el).removeClass(cls)
			}
		}
	}

	MS.buildSelect("#para-select", {
		onChange: function(options){
			//console.log(options)
			if(editor){
				editor.focus();
				var el = editor.getParagraph();
				var something_done=false;
				for(let para in options){
					if(options[para]){
						editor.execCmd("FormatBlock", para||null);
						something_done=true
						break;
					}
				}
				if(!something_done && el)
					editor.removeNode(el, true);
			}
		},
		nonSelectedText: "Kein Absatz"
	})

	MS.buildSelect("#text-style-ms", {
		onChange: function(options){
			editor.focus();
			var el = editor.getParagraph();
			if(!el)
				editor.execCmd("FormatBlock", "p");
			setClasses(editor.getParagraph(), options);
		},
		multiselect: true,
		nonSelectedText: "Kein Stil ausgewählt",
		nSelectedText: "Stile ausgewählt"
	})

	MS.buildSelect("#table-style-ms", {
		nonSelectedText: "Kein Stil ausgewählt",
		nSelectedText: "Stile ausgwewählt",
		buttonClass: "btn-sm",
		multiselect: true,
		onChange: function(options){
			var el = editor.getNearestTagFromSelection("table")
			setClasses(el, options);
		}
	})

	MS.buildSelect("#link-style-ms", {
		nonSelectedText: "Kein Stil ausgewählt",
		nSelectedText: "Stile ausgwewählt",
		buttonClass: "btn-sm",
		multiselect: true,
		onChange: function(options){
			var el = editor.getNearestTagFromSelection("a")
			setClasses(el, options);
		}
	})

	MS.buildSelect("#image-style-ms", {
		nonSelectedText: "Kein Stil ausgewählt",
		nSelectedText: "Stile ausgwewählt",
		buttonClass: "btn-sm",
		multiselect: true,
		onChange: function(options){
			var el = editor.getNearestTagFromSelection("img")
			setClasses(el, options);
		}		
	})

	MS.buildSelect("#list-style-ms", {
		nonSelectedText: "Kein Listenstil ausgewählt",
		nSelectedText: "Stile ausgwewählt",
		buttonClass: "btn-sm",
		multiselect: true,
		onChange: function(options){
			var el = editor.getNearestTagFromSelection("ul")
			setClasses(el, options);
		}
	})
	
	$("#editor-panel-rtf textarea").autogrow();
	
	// attach click handler
	$("#editor-panel-rtf").on("click", "[data-cmd]", function(ev){
		ev.preventDefault();
		$this = $(this);
		if(editor){
			//editor.focus();
			editor.execCmd($this.data("cmd"), $this.data("param")||null);
			editor.focus();
		}
	})
	$("#editor-panel-rtf").on("click", "[data-action]", function(ev){
		var button = $(ev.target).closest("[data-action]")
		var action = button.data("action");
		if(actions[action]){
			ev.preventDefault();
			actions[action](button);
		}
	})

	var actions = {
		
		"clean-html": function(){
			if(confirm("Möchten Sie wirlich das HTML bereinigen und damit ggfl. Formattierungen entfernen?"))
				editor.cleanHTML()						
		},
		
		"create-table": function(){
			editor.insertHTML('<table style="width:100%"><tr><td><br></td><td><br></td></tr><tr><td><br></td><td><br></td></tr></table>');			
		},
		"delete-table": function(){
			var el = editor.getNearestTagFromSelection("table")
			el && editor.deleteElement(el);
			editor.focus();
		},
		"create-td": function(){
			editor.insertTableCol();
			editor.focus();
		},
		"create-tr": function(){
			editor.insertTableRow();
			editor.focus();
		},
		"delete-td": function(){
			editor.deleteTableCol();
			editor.focus();
		},
		"delete-tr": function(){
			editor.deleteTableRow()
			editor.focus();
		},
		"table-para-before": function(){
			var el = editor.getNearestTagFromSelection("table")
			el && $("<p><br></p>").insertBefore(el);
			editor.focus();
		},
		"table-para-after": function(){
			var el = editor.getNearestTagFromSelection("table")
			el && $("<p><br></p>").insertAfter(el);
			editor.focus();
		},
		
		"create-link": function(el){
			var a = editor.getNearestTagFromSelection("a")
			if(a)
				return; // should not happen 
			CM.openDialog("edit-rtf-link", {
				target_window: "default"
			})
		},
		"edit-link": function(el){
			var a = editor.getNearestTagFromSelection("a")
			if(!a)
				return;
			var info = editor.getURLInfo(a)
			var key = info.key;
			var anker = ""
			if(info.type=="int"){
				var parts = key.split("#");
				key = parts[0];
				anker = parts[1];
			}
			CM.openDialog("edit-rtf-link", {
				title: a.title,
				target: a.target,
				target_window: a.dataset.target,
				type: info.type,
				key: key,
				anker: anker,
				url: a.href
			});			
		},
		"delete-link": function(el){
			var a = editor.getNearestTagFromSelection("a")
			a && editor.removeLink(a)
			editor.focus();
		},

		"edit-image": function(el){
			var el = editor.getNearestTagFromSelection("img")
			if(!el)
				return;
			var info = editor.getURLInfo(el)
			CM.openDialog("edit-rtf-image", {
				title: el.title,
				type: info.type,
				key: info.key,
				url: el.src,
				alt: el.alt
			});			
		},
		"create-image": function(el){
			var el = editor.getNearestTagFromSelection("img")
			if(el)
				return; // should not happen 
			CM.openDialog("edit-rtf-image")
		},
		
		"edit-html": function(el){
			CM.openDialog("edit-html", {
				html: editor.html()
			});
		},

		"chatgpt": function(el){
			CM.openDialog("chatgpt");
		},
		
		"remove-image-styles": function(){
			var el = editor.getNearestTagFromSelection("img")
			if(el){
				el.removeAttribute("style");
				editor.focus();
			}
		}
	
	}

	$("#rtf-tab-image [name=width]").on({
		keyup: function(ev){
			if(!selectedImg)
				return;
			var img = $(selectedImg);
			var h_el = $("#rtf-tab-image [name=height]")
			var w_el = $(this);
			var value = w_el.val();
			if(ev.keyCode==40)
				value--;
			else if(ev.keyCode==38)
				value++;
			
			img.removeAttr("height");
			img.removeAttr("width");
			
			if(!value){
				img.css("width", "");
				img.css("height", "");
			}
			else if(value != img.outerWidth()){
				img.css("width", value);
				img.css("height", "auto");
				if(value != img.outerWidth())
					img.css("width", "");
			}
			
			h_el.css("color", img.prop("style").height && img.prop("style").height!="auto" ? "brown" : "gray")
			w_el.css("color", img.prop("style").width && img.prop("style").width!="auto" ? "brown" : "gray")
			
			if(h_el.val() != img.outerHeight())
				h_el.val(parseInt(img.outerHeight()));
			if(w_el.val() != img.outerWidth())
				w_el.val(parseInt(img.outerWidth()));
		}
	})
	$("#rtf-tab-image [name=height]").on({
		keyup: function(ev){
			if(!selectedImg)
				return;
			var img = $(selectedImg);
			var w_el = $("#rtf-tab-image [name=width]")
			var h_el = $(this);
			var value = h_el.val();
			if(ev.keyCode==40)
				value--;
			else if(ev.keyCode==38)
				value++;
			
			img.removeAttr("height");
			img.removeAttr("width");
			
			if(!value){
				img.css("height", "");
				img.css("width", "");
			}
			else if(value != img.outerHeight()){
				img.css("height", value);
				img.css("width", "auto");
				if(value != img.outerHeight())
					img.css("height", "");
			}
			
			h_el.css("color", img.prop("style").height && img.prop("style").height!="auto" ? "brown" : "gray")
			w_el.css("color", img.prop("style").width && img.prop("style").width!="auto" ? "brown" : "gray")
			
			if(h_el.val() != img.outerHeight())
				h_el.val(parseInt(img.outerHeight()));
			if(w_el.val() != img.outerWidth())
				w_el.val(parseInt(img.outerWidth()));
		}
	})
	
	var toolbar = {
		update: function(){
			$("#editor-panel-rtf [data-cmd]").each(function(){
				$this = $(this)
				try{
					var enabled = editor.queryCommandState($this.data("cmd"));
					$this[enabled ? "addClass" : "removeClass"]("active");
				}
				catch(e){
					console.log(e);
				}
			})
			
			// paragraph
			var el = editor.getParagraph()
			if(el){
				MS.select("#text-style-ms", el.className.split(" "));
				//MS.disable("#text-style-ms", false);
				MS.select("#para-select", [el.tagName.toLowerCase()])
			}
			else{
				MS.select("#text-style-ms", []);
				//MS.disable("#text-style-ms", true);
				MS.select("#para-select", [])
			}
			
			// table
			var el = editor.getNearestTagFromSelection("table")
			if(el){
				$("#editor-panel-rtf [data-action=create-table]").hide()
				$("#editor-panel-rtf .table-edit-actions").show()
				var classes = el.className.split(" ");
				if(options && options.tableStyleList && options.tableStyleList.length){
					MS.select("#table-style-ms", classes);
				}
			}
			else{
				$("#editor-panel-rtf [data-action=create-table]").show()
				$("#editor-panel-rtf .table-edit-actions").hide()				
			}
			
			// links
			var el = editor.getNearestTagFromSelection("a")
			if(el){
				$("#editor-panel-rtf [data-action=create-link]").hide()
				$("#editor-panel-rtf [data-action=delete-link]").show()
				$("#editor-panel-rtf [data-action=edit-link]").show()
				$("#editor-panel-rtf [data-id=link-info-wrapper]").show()
				
				var classes = el.className.split(" ");
				if(options && options.linkStyleList && options.linkStyleList.length){					
					$("#editor-panel-rtf .link-options").show()
					MS.select("#link-style-ms", classes);
				}
				var info = editor.getURLInfo(el)
				var types={
					"int": "Interner Link",
					"exturl": "Externer Link",
					"mailto": "Mail",
					"intfile": "Link auf Datei",
					"extfile": "Link auf externe Datei",
					"scriptlet": "Projekt-Link",
					"undefined": "Undefiniert"
				}
				$("#editor-panel-rtf [data-id=link-type]").html(types[info.type||"undefined"])
				if(info.type=="exturl")
					$("#editor-panel-rtf [data-id=link-info]").html(info.key || $(el).prop("href"))
				else if(info.type=="intfile" || info.type=="mailto" || info.type=="scriptlet")
					$("#editor-panel-rtf [data-id=link-info]").html(info.key)
				else if(info.type=="extfile"){
					var parts = info.key.split("/");
					var filename = parts[1];
					$("#editor-panel-rtf [data-id=link-info]").html(filename)
				}
				else $("#editor-panel-rtf [data-id=link-info]").html("")
			}
			else{
				$("#editor-panel-rtf [data-id=link-info-wrapper]").hide()			
				$("#editor-panel-rtf [data-action=delete-link]").hide()
				$("#editor-panel-rtf [data-action=edit-link]").hide()
				$("#editor-panel-rtf .link-options").hide()

				$("#editor-panel-rtf [data-action=create-link]").show()
			}

			// List
			var el = editor.getNearestTagFromSelection("ul")
			if(el){
				var classes = el.className.split(" ");
				if(options && options.listStyleList && options.listStyleList.length){
					MS.select("#list-style-ms", classes);					
					$("#editor-panel-rtf .list-options").show()
				}
			}
			else $("#editor-panel-rtf .list-options").hide()
			
			// Image
			var el = editor.getNearestTagFromSelection("img")
			selectedImg=el;
			if(el){
				$("#rtf-tab-image [data-action=create-image]").hide()
				$("#rtf-tab-image [data-action=edit-image]").show()
				$("#editor-panel-rtf .img-options").show()

				if(el.style.length)
					$("#rtf-tab-image [data-action=remove-image-styles]").show()
				else $("#rtf-tab-image [data-action=remove-image-styles]").hide()
				
				if(options.customImageSizes===false)
					$("#rtf-tab-image [data-id=image-size]").hide()
				else $("#rtf-tab-image [data-id=image-size]").show()
				
				$("#editor-panel-rtf [data-id=image-info-wrapper]").show()

				$("#rtf-tab-image [name=width]").val(parseInt(el.width)).css("color", el.style.width && el.style.width!="auto" ? "brown":"gray")
				$("#rtf-tab-image [name=height]").val(parseInt(el.height)).css("color", el.style.height && el.style.height!="auto" ? "brown":"gray")
				
				var classes = el.className.split(" ");
				if(options && options.imageStyleList && options.imageStyleList.length){
					$("#editor-panel-rtf [data-id=image-style]").show()
					MS.select("#image-style-ms", classes);					
				}
				else $("#editor-panel-rtf [data-id=image-style]").hide()
				
				var info = editor.getURLInfo(el)
				var types={
					"exturl": "Externes Bild",
					"intfile": "Internes Bild",
					"extfile": "Bild aus anderem Dokument",
					"undefined": "Undefiniert"
				}
				var type = types[info.type||"undefined"]
				
				if(info.type=="exturl"){
					var src = $(el).prop("src")
					if(src.indexOf("data:")==0){
						type = "Eingebettete Bilddaten";
						src="";
					}
					$("#editor-panel-rtf [data-id=image-info]").html(src)
				}
				else if(info.type=="intfile")
					$("#editor-panel-rtf [data-id=image-info]").html(info.key)
				else if(info.type=="extfile"){
					var parts = info.key.split("/");
					var filename = parts[1];
					$("#editor-panel-rtf [data-id=image-info]").html(filename)
				}
				else $("#editor-panel-rtf [data-id=image-info]").html("")
				$("#editor-panel-rtf [data-id=image-type]").html(type)
			}
			else{
				$("#rtf-tab-image [data-action=edit-image]").hide()
				$("#rtf-tab-image [data-action=remove-image-styles]").hide()
				$("#editor-panel-rtf [data-id=image-info-wrapper]").hide()
				$("#rtf-tab-image [data-action=create-image]").show()
				
				$("#editor-panel-rtf .img-options").hide()
			}
		
		},
		disableAllButtons: function(disable){
			console.log("disableAllButtons", disable);
		},
		isCmdDisabled: function(){
			return false;
		}
	}

	return{
		setEditor: function(e){
			editor=e;
			editor.toolbar=toolbar;
			editor.focus();
			toolbar.update();
		},
		getEditor: function(){
			return editor;
		},
		setOptions: function(opts){
			options = opts;
			if(options.hideoptions){
				for(var i=0; i<options.hideoptions.length; i++){
					var opt = options.hideoptions[i];
					$("#editor-panel-rtf [data-cmd=" + opt + "]").hide();
				}
				// special handlings
				if(options.hideoptions.indexOf("editHTML")>=0)
					$("#editor-panel-rtf [data-action=edit-html]").hide();
				if(options.hideoptions.indexOf("InsertTable")>=0)
					$("#editor-panel-rtf .InsertTable").hide();
			}
			
			// headings
			var headings = {
				"h1": "Überschrift 1",
				"h2": "Überschrift 2",
				"h3": "Überschrift 3",
				"h4": "Überschrift 4",
				"h5": "Überschrift 5",
				"h6": "Überschrift 6"
			}
			heading_opts=["normaler Absatz|p"]
			var add_divider=true;
			for(let heading in headings){
				if(!options.hideoptions || options.hideoptions.indexOf(heading)<0){
					if(add_divider){
						heading_opts.push("-")
						add_divider=false;
					}
					heading_opts.push(headings[heading] + "|" + heading)
				}
			}
			if(!options.hideoptions || options.hideoptions.indexOf("pre")<0){
				heading_opts.push("-")
				heading_opts.push("PRE|pre")
			}
			MS.buildOptions("#para-select", heading_opts)
			
			if(options && options.paragraphStyleList && options.paragraphStyleList.length){
				toolbar.paragraphStyleList = MS.buildOptions("#text-style-ms", options.paragraphStyleList);
			}
			
			if(options && options.linkStyleList && options.linkStyleList.length){
				toolbar.linkStyleList = MS.buildOptions("#link-style-ms", options.linkStyleList);
			}
			
			if(options && options.imageStyleList && options.imageStyleList.length){
				toolbar.imageStyleList = MS.buildOptions("#image-style-ms", options.imageStyleList);
			}

			if(options && options.listStyleList && options.listStyleList.length){
				toolbar.listStyleList = MS.buildOptions("#list-style-ms", options.listStyleList);
			}

			if(options && options.tableStyleList && options.tableStyleList.length){
				toolbar.tableStyleList = MS.buildOptions("#table-style-ms", options.tableStyleList);
			}
		}
		
	}

})