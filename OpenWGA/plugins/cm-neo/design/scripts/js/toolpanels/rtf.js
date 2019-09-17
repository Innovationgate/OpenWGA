define(["jquery", "cm", "afw/rtfeditor", "bootstrap-multiselect"], function($, CM){

	var editor;
	var options;
	var selectedImg;

	$("#editor-panel-rtf textarea").autogrow();
	
	$("#editor-panel-rtf [name=para]").multiselect({
		nonSelectedText: "Kein Absatz",
		buttonWidth: '100%',
		onChange: function(option){
			if(editor){
				editor.focus();
				var para = $(option).val()
				if(para){
					editor.execCmd("FormatBlock", para||null);
				}
				else {
					var el = editor.getParagraph();
					if(el){
						editor.removeNode(el, true);
					}
				}
			}
		}
	})
	
	$("#editor-panel-rtf [name=text-style]").multiselect({
		nonSelectedText: "Kein Stil ausgewählt",
		numberDisplayed: 2,
		nSelectedText: "Stile ausgwewählt",
		allSelectedText: "Alle Stile ausgewählt",
		disableIfEmpty: true,
		buttonWidth: '100%',
		onChange: function(option, checked){
			editor.focus();
			var el = editor.getParagraph()
			var cls = $(option).val()
			if(el){
				if(checked)
					$(el).addClass(cls)
				else $(el).removeClass(cls)
				if(editor.toolbar)
					editor.toolbar.update();
			}
		}
	});

	$("#editor-panel-rtf [name=table-style]").multiselect({
		nonSelectedText: "Kein Stil ausgewählt",
		numberDisplayed: 2,
		nSelectedText: "Stile ausgwewählt",
		allSelectedText: "Alle Stile ausgewählt",
		buttonClass: "btn btn-default btn-sm",
		disableIfEmpty: true,
		buttonWidth: '100%',
		onChange: function(option, checked){
			var el = editor.getNearestTagFromSelection("table")
			var cls = $(option).val()
			if(el){
				if(checked)
					$(el).addClass(cls)
				else $(el).removeClass(cls)
			}
		}
	});

	$("#editor-panel-rtf [name=link-style]").multiselect({
		nonSelectedText: "Kein Stil ausgewählt",
		numberDisplayed: 2,
		nSelectedText: "Stile ausgwewählt",
		allSelectedText: "Alle Stile ausgewählt",
		buttonClass: "btn btn-default btn-sm",
		disableIfEmpty: true,
		buttonWidth: '100%',
		onChange: function(option, checked){
			var el = editor.getNearestTagFromSelection("a")
			var cls = $(option).val()
			if(el){
				if(checked)
					$(el).addClass(cls)
				else $(el).removeClass(cls)
			}
		}
	});

	$("#editor-panel-rtf [name=image-style]").multiselect({
		nonSelectedText: "Kein Stil ausgewählt",
		numberDisplayed: 2,
		nSelectedText: "Stile ausgwewählt",
		allSelectedText: "Alle Stile ausgewählt",
		buttonClass: "btn btn-default btn-sm",
		disableIfEmpty: true,
		buttonWidth: '100%',
		onChange: function(option, checked){
			var el = editor.getNearestTagFromSelection("img")
			var cls = $(option).val()
			if(el){
				if(checked)
					$(el).addClass(cls)
				else $(el).removeClass(cls)
			}
		}
	});

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
			CM.openDialog("edit-rtf-link")
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
				url: el.src
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
			}
			else if(value != img.width()){
				img.css("width", value);
				img.css("height", "");
				if(value != img.width())
					img.css("width", "");
			}
			
			h_el.css("color", img.prop("style").height ? "brown" : "gray")
			w_el.css("color", img.prop("style").width ? "brown" : "gray")
			
			if(h_el.val() != img.height())
				h_el.val(img.height());
			if(w_el.val() != img.width())
				w_el.val(img.width());
		}
	})
	$("#rtf-tab-image [name=height]").on({
		keyup: function(){
			if(!selectedImg)
				return;
			var img = $(selectedImg);
			var w_el = $("#rtf-tab-image [name=width]")
			var h_el = $(this);
			var value = h_el.val();
			
			img.removeAttr("height");
			img.removeAttr("width");
			
			if(!value){
				img.css("height", "");
			}
			else if(value != img.height()){
				img.css("height", value);
				img.css("width", "");
				if(value != img.height())
					img.css("height", "");
			}
			
			h_el.css("color", img.prop("style").height ? "brown" : "gray")
			w_el.css("color", img.prop("style").width ? "brown" : "gray")
			
			if(h_el.val() != img.height())
				h_el.val(img.height());
			if(w_el.val() != img.width())
				w_el.val(img.width());
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
			$("#editor-panel-rtf [name=para]").multiselect('deselectAll', false)
			$("#editor-panel-rtf [name=text-style]").multiselect('deselectAll', false)
			if(el){
				$("#editor-panel-rtf [name=para]").multiselect('select', el.tagName.toLowerCase())

				var classes = el.className.split(" ");
				if(options && options.paragraphStyleList && options.paragraphStyleList.length){
					$("#editor-panel-rtf [name=text-style]").multiselect('select', classes)
					$("#editor-panel-rtf [name=text-style]").multiselect('enable')
				}
			}
			else{
				$("#editor-panel-rtf [name=para]").multiselect('select', "")
				$("#editor-panel-rtf [name=text-style]").multiselect('disable')
				$("#editor-panel-rtf [name=text-style]").multiselect('updateButtonText')
			}
			
			// table
			$("#editor-panel-rtf [name=table-style]").multiselect('deselectAll', false)
			var el = editor.getNearestTagFromSelection("table")
			if(el){
				$("#editor-panel-rtf [data-action=create-table]").hide()
				$("#editor-panel-rtf .table-edit-actions").show()
				var classes = el.className.split(" ");
				if(options && options.tableStyleList && options.tableStyleList.length){
					$("#editor-panel-rtf [name=table-style]").multiselect('select', classes)
				}
			}
			else{
				$("#editor-panel-rtf [data-action=create-table]").show()
				$("#editor-panel-rtf .table-edit-actions").hide()				
			}
			
			// links
			$("#editor-panel-rtf [name=link-style]").multiselect('deselectAll', false)
			var el = editor.getNearestTagFromSelection("a")
			if(el){
				$("#editor-panel-rtf [data-action=create-link]").hide()
				$("#editor-panel-rtf [data-action=delete-link]").show()
				$("#editor-panel-rtf [data-action=edit-link]").show()
				$("#editor-panel-rtf [data-id=link-info-wrapper]").show()
				
				var classes = el.className.split(" ");
				if(options && options.linkStyleList && options.linkStyleList.length){
					$("#editor-panel-rtf .link-options").show()
					$("#editor-panel-rtf [name=link-style]").multiselect('select', classes)
				}
				var info = editor.getURLInfo(el)
				var types={
					"int": "Interner Link",
					"exturl": "Externer Link",
					"intfile": "Link auf Datei",
					"extfile": "Link auf externe Datei",
					"undefined": "Undefiniert"
				}				
				$("#editor-panel-rtf [data-id=link-type]").html(types[info.type||"undefined"])
				if(info.type=="exturl")
					$("#editor-panel-rtf [data-id=link-info]").html(info.key || $(el).prop("href"))
				else if(info.type=="intfile")
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

			// Image
			var el = editor.getNearestTagFromSelection("img")
			$("#editor-panel-rtf [name=image-style]").multiselect('deselectAll', false)
			selectedImg=el;
			if(el){
				$("#rtf-tab-image [data-action=create-image]").hide()
				$("#rtf-tab-image [data-action=edit-image]").show()
				$("#editor-panel-rtf .img-options").show()
				$("#editor-panel-rtf [data-id=image-info-wrapper]").show()
				
				$("#rtf-tab-image [name=width]").val(el.width).css("color", el.style.width?"brown":"gray")
				$("#rtf-tab-image [name=height]").val(el.height).css("color", el.style.height?"brown":"gray")

				var classes = el.className.split(" ");
				if(options && options.imageStyleList && options.imageStyleList.length){
					$("#editor-panel-rtf [data-id=image-style]").show()
					$("#editor-panel-rtf [name=image-style]").multiselect('select', classes)					
				}
				var info = editor.getURLInfo(el)
				var types={
					"exturl": "Externes Bild",
					"intfile": "Internes Bild",
					"extfile": "Bild aus anderem Dokument",
					"undefined": "Undefiniert"
				}				
				$("#editor-panel-rtf [data-id=image-type]").html(types[info.type||"undefined"])
				if(info.type=="exturl")
					$("#editor-panel-rtf [data-id=image-info]").html($(el).prop("src"))
				else if(info.type=="intfile")
					$("#editor-panel-rtf [data-id=image-info]").html(info.key)
				else if(info.type=="extfile"){
					var parts = info.key.split("/");
					var filename = parts[1];
					$("#editor-panel-rtf [data-id=image-info]").html(filename)
				}
				else $("#editor-panel-rtf [data-id=image-info]").html("")
			}
			else{
				$("#rtf-tab-image [data-action=edit-image]").hide()
				$("#editor-panel-rtf [data-id=image-info-wrapper]").hide()
				$("#rtf-tab-image [data-action=create-image]").show()
				$("#rtf-tab-image [name=width]").val("").css("color", "black")
				$("#rtf-tab-image [name=height]").val("").css("color", "black")
				
				$("#editor-panel-rtf .img-options").hide()
				$("#editor-panel-rtf [data-id=image-style]").show()
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
			options = opts
			if(options && options.paragraphStyleList && options.paragraphStyleList.length){
				var opts=[]
				toolbar.paragraphStyleList = options.paragraphStyleList;
				for(var i=0; i<options.paragraphStyleList.length; i++){
					var parts = options.paragraphStyleList[i].split("|");
					opts.push({
						label: parts[0],
						title: parts[0],
						value: parts[1]
					})
				}
				$("#editor-panel-rtf [name=text-style]").multiselect("dataprovider", opts)
			}
			if(options && options.linkStyleList && options.linkStyleList.length){
				opts=[]
				toolbar.linkStyleList = options.linkStyleList;
				for(var i=0; i<options.linkStyleList.length; i++){
					var parts = options.linkStyleList[i].split("|");
					opts.push({
						label: parts[0],
						title: parts[0],
						value: parts[1]
					})
				}
				$("#editor-panel-rtf [name=link-style]").multiselect("dataprovider", opts)
			}
			if(options && options.imageStyleList && options.imageStyleList.length){
				opts=[]
				toolbar.imageStyleList = options.imageStyleList;
				for(var i=0; i<options.imageStyleList.length; i++){
					var parts = options.imageStyleList[i].split("|");
					opts.push({
						label: parts[0],
						title: parts[0],
						value: parts[1]
					})
				}
				$("#editor-panel-rtf [name=image-style]").multiselect("dataprovider", opts)
			}
			if(options && options.tableStyleList && options.tableStyleList.length){
				opts=[]
				toolbar.tableStyleList = options.tableStyleList;
				for(var i=0; i<options.tableStyleList.length; i++){
					var parts = options.tableStyleList[i].split("|");
					opts.push({
						label: parts[0],
						title: parts[0],
						value: parts[1]
					})
				}
				$("#editor-panel-rtf [name=table-style]").multiselect("dataprovider", opts)
			}
		}
		
	}

})