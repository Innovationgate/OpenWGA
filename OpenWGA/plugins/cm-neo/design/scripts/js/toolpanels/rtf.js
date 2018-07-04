define(["jquery", "cm", "bootstrap-multiselect"], function($, CM){

	var editor;
	var options;
	var selectedImg;

	$("#editor-panel-rtf textarea").autogrow();
	
	$("#editor-panel-rtf [name=para]").multiselect({
		nonSelectedText: "Kein Absatz",
		buttonWidth: '100%',
		onChange: function(option){
			if(editor){
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
				editor.focus();
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
			var el = editor.getParagraph()
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
			if(confirm("Möchten Sie wirlich das HTML bereinigen und damit Formattierungen entfernen?"))
				editor.cleanHTML()						
		},
		
		"create-link": function(el){
			var a = editor.getNearestTagFromSelection("a")
			if(a)
				return; // should not happen 
			var root_el = $("#rtf-tab-link");
			var url = $("[name=url]", root_el).val();
			if(!url){
				alert("Bitte geben Sie eine Link-URL ein.");
				$("[name=url]", root_el).focus();
				return;
			}
			a = editor.createLink(url, url, "exturl");
			AFW.RTF.setURLInfo(a, {type:"exturl", key:url})
			a.title = $("[name=title]", root_el).val();
			var target = $("[name=target]", root_el).val();
			if(target)
				a.target = target;
			else a.removeAttribute("target") 
		},
		"update-link": function(el){
			var a = editor.getNearestTagFromSelection("a")
			if(!a)
				return;
			var root_el = $("#rtf-tab-link");
			var info = AFW.RTF.getURLInfo(a)
			if(info.type=="int" || info.type=="intfile"){
				var key = info.key;
				var parts = key.split("#");
				var anker = $("[name=anker]", root_el).val();
				AFW.RTF.setURLInfo(a, {
					type:info.type, 
					key:parts[0] + (anker ? "#"+anker : "")
				})
			}
			else{
				var url = $("[name=url]", root_el).val();				
				AFW.RTF.setURLInfo(a, {type:"exturl", key:url})
			}
			a.title = $("[name=title]", root_el).val();
			var target = $("[name=target]", root_el).val();
			if(target)
				a.target = target;
			else a.removeAttribute("target") 
		},
		"delete-link": function(el){
			var a = editor.getNearestTagFromSelection("a")
			//console.log("delete link", a)
			a && editor.removeLink(a)
			editor.focus();
		},

		"create-image": function(el){
			var el = editor.getNearestTagFromSelection("img")
			if(el)
				return; // should not happen 
			var root_el = $("#rtf-tab-image");
			var url = $("[name=url]", root_el).val();
			if(!url){
				alert("Bitte geben Sie eine Bild-URL ein.");
				$("[name=url]", root_el).focus();
				return;
			}
			el = editor.createImg(url, "exturl");
			AFW.RTF.setURLInfo(el, {type:"exturl", key:url})
			el.alt = el.title = $("[name=title]", root_el).val();
		},
		"update-image": function(el){
			var el = editor.getNearestTagFromSelection("img")
			if(!el)
				return;
			var root_el = $("#rtf-tab-image");
			if(AFW.RTF.getURLInfo(el).type!="intfile"){
				var url = $("[name=url]", root_el).val();
				el.src=url;
				AFW.RTF.setURLInfo(el, {type:"exturl", key:url})
			}
			el.alt = el.title = $("[name=title]", root_el).val();
		},
		
		"edit-html": function(el){
			CM.openDialog("edit-html", {
				html: editor.getHTML()
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
		update: function(event){
			$("#editor-panel-rtf [data-cmd]").each(function(){
				$this = $(this)
				try{
					var enabled = editor.doc.queryCommandState($this.data("cmd"));
					$this[enabled ? "addClass" : "removeClass"]("active");
				}
				catch(e){}
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
			
			// links
			$("#editor-panel-rtf [name=link-style]").multiselect('deselectAll', false)
			var el = editor.getNearestTagFromSelection("a")
			if(el){
				$("#editor-panel-rtf [data-action=create-link]").hide()
				$("#editor-panel-rtf [data-action=update-link]").show()
				$("#editor-panel-rtf [data-action=delete-link]").show()
				
				$("#rtf-tab-link [name=url]").val($(el).prop("href"))
				$("#editor-panel-rtf [name=title]").val(el.title)
				$("#editor-panel-rtf [name=target]").val(el.target)

				var info = AFW.RTF.getURLInfo(el)
				if(info.type=="int" || info.type=="intfile"){
					$("#editor-panel-rtf [data-id=anker-wrapper]").show()
					var parts = info.key.split("#");
					$("#editor-panel-rtf [name=url]").val(parts[0])
						.prop("disabled", true);
					$("#editor-panel-rtf [name=anker]").val(parts[1])
				}
				else {
					$("#editor-panel-rtf [data-id=anker-wrapper]").hide()
					$("#editor-panel-rtf [name=url]").val(info.key)
						.prop("disabled", false);
				}
				
				var classes = el.className.split(" ");
				if(options && options.linkStyleList && options.linkStyleList.length){
					$("#editor-panel-rtf .link-options").show()
					$("#editor-panel-rtf [name=link-style]").multiselect('select', classes)
				}
			}
			else{
				$("#editor-panel-rtf [name=url]").val("").prop("disabled", false);
				$("#editor-panel-rtf [name=anker]").val("")
				$("#editor-panel-rtf [name=title]").val("")
				$("#editor-panel-rtf [name=target]").val("")
				$("#editor-panel-rtf [data-id=link-type]").html("Kein Link")
				$("#editor-panel-rtf [data-id=url-wrapper]").show()
				$("#editor-panel-rtf [data-action=update-link]").hide()
				$("#editor-panel-rtf [data-action=delete-link]").hide()
				$("#editor-panel-rtf [data-action=create-link]").show()

				$("#editor-panel-rtf .link-options").hide()
				$("#editor-panel-rtf [data-id=anker-wrapper]").hide()
			}

			// Image
			var el = editor.getNearestTagFromSelection("img")
			$("#editor-panel-rtf [name=image-style]").multiselect('deselectAll', false)
			selectedImg=el;
			if(el){
				$("#rtf-tab-image [data-action=update-image]").show()
				$("#rtf-tab-image [data-action=create-image]").hide()
				var link_type=AFW.RTF.getURLInfo(el).type;
				if(link_type=="intfile")
					$("#rtf-tab-image [name=url]").val(AFW.RTF.getURLInfo(el).key).prop("disabled", true).autogrow("update");
				else {
					$("#rtf-tab-image [name=url]").val($(el).prop("src")).prop("disabled", false).autogrow("update");
				}
				$("#rtf-tab-image [name=title]").val(el.title)
				$("#rtf-tab-image [name=width]").val(el.width).css("color", el.style.width?"brown":"gray")
				$("#rtf-tab-image [name=height]").val(el.height).css("color", el.style.height?"brown":"gray")

				var classes = el.className.split(" ");
				if(options && options.imageStyleList && options.imageStyleList.length){
					$("#editor-panel-rtf .img-options").show()
					$("#editor-panel-rtf [name=image-style]").multiselect('select', classes)					
				}
			}
			else{
				$("#rtf-tab-image [data-action=update-image]").hide()
				$("#rtf-tab-image [data-action=create-image]").show()
				$("#rtf-tab-image [name=url]").val("").prop("disabled", false).autogrow("update");
				$("#rtf-tab-image [name=title]").val("");
				$("#rtf-tab-image [name=width]").val("").css("color", "black")
				$("#rtf-tab-image [name=height]").val("").css("color", "black")
				
				//$("#editor-panel-rtf [name=image-style]").multiselect('select', [])
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

	function delay(f, timeout){
		var timer = null;
		return function(){
			var self = this;
			var args = arguments;
			if(timer)
				clearTimeout(timer);
			timer = setTimeout(function(){
				f.apply(self, args)
			}, timeout||500)
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
		}
		
	}

})