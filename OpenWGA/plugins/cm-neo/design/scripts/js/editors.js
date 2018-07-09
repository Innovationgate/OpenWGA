define([
	"sitepanel",
	"uploadmanager",
	"cm",
	"jquery-textarea-autogrow",
	"/plugin-wga-app-framework/js/rtfeditor2"		// defines AFW.RTF.editor
], function(Sitepanel, UploadManager, CM){

	var rtf_editor = function(){
	
		var editor;
	
		function open(item, option){
			
			//console.log("open RTF editor", option);
			Sitepanel.getWindow().onbeforeunload=function(){
				return "Daten wurden noch nicht gespeichert";
			}
			
			editor = new AFW.RTF.editor("item_"+item, {
				document: Sitepanel.getDocument(),
				contentinfo: Sitepanel.getContentInfo(),
				width: "100%"
			})

			// drag&drop
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

			function isLinkDrop(dt){
				return dt.types.indexOf ? (dt.types.indexOf("wga/link")>=0) 
					: dt.types.contains ? (dt.types.contains("wga/link"))
					: false;
			}
			
			function forceCreateLink(){
				if(editor.getNearestTagFromSelection("img"))
					return false;
				else if(editor.getNearestTagFromSelection("a"))
					return true;
				else return editor.getSelectedText()!=""
			}
			
			function setDropEffect(e){
				var dt = e.dataTransfer;
				if(isLinkDrop(dt))
					dt.dropEffect = "link";				
				else if(!isFilesDrop(dt) && !isDesktopDrop(dt))
					dt.dropEffect = "none"
				else dt.dropEffect = (e.shiftKey||forceCreateLink() ? "link":"copy");
			}
			
			function ignoreEvent(e) {
			    e.preventDefault();
			}
			function dragover(e) {
				setDropEffect(e);
			    e.preventDefault();
			    //console.log("rtf dragover")
			}
			function dragenter(e) {
				setDropEffect(e);
				editor.focus();
			    e.preventDefault();
			    //console.log("rtf dragenter")
			}
			
			function drop(e) {
				
				//console.log("rtf drop")
				
				if(!e.dataTransfer){
					return;
				}
				setDropEffect(e);

			    e.preventDefault();
			    e.stopPropagation();
			    
			    if(e.dataTransfer.getData("wga/link")){
			    	var data = JSON.parse(e.dataTransfer.getData("wga/link"))
			    	//console.log("link drop", e.dataTransfer.getData("wga/link"))
					var el = editor.createLink(data.href, data.title, "int");
					AFW.RTF.setURLInfo(el, {type:"int", key:data.id})
					el.alt = el.title = data.title;
					editor.getRange().setStartAfter(el);
			    	return;
			    }
			    
				if(e.dataTransfer.getData("wga/files")){
					var data = JSON.parse(e.dataTransfer.getData("wga/files"))
					if(forceCreateLink()){
						insertAttachment(data[0], true);
					}
					else{
						for(var i=0; i<data.length; i++)
							insertAttachment(data[i], e.dataTransfer.dropEffect=="link");
					} 
					return;
				}
				if(!e.dataTransfer.files || e.dataTransfer.files.length==0){
					return;
				}
			    
				if(!editor.toolbar)
					return;
					
				// Files drop from local file system:
				if(forceCreateLink()){
					if(!editor.toolbar.isCmdDisabled("InsertLink"))
						handleFile(e.dataTransfer.files[0], "link");
				}
				else{
				    for(var i=0; i<e.dataTransfer.files.length; i++){
					    var file = e.dataTransfer.files[i];
					    
						// check if images/links are allowed in RTF config
						if(e.dataTransfer.dropEffect=="copy" && file.type.indexOf("image/")==0){
							// create image ?
							if(editor.toolbar.isCmdDisabled("InsertImg")){
								if(editor.toolbar.isCmdDisabled("InsertLink"))
									return;		// no images and no links allowed
								if (confirm("Images are not allowed. Do you want to create a link instead?"))
									handleFile(file, "link");
								else return;
							}
							//else handleFile.defer(100, this, [file, "image"]);
							else handleFile(file, "image");
						}
						else if(!editor.toolbar.isCmdDisabled("InsertLink"))
							handleFile(file, "link");
			    	}
			    }
			}
			
			function insertAttachment(file, as_link){
				if(as_link && !editor.toolbar.isCmdDisabled("InsertLink")){
					var el = editor.createLink(file.url, file.name, file.type||"intfile");
					AFW.RTF.setURLInfo(el, {type:file.type||"intfile", key:file.key||file.name})
					el.alt = el.title=file.title || file.name;
					editor.getRange().setStartAfter(el);
				}
				else if(!editor.toolbar.isCmdDisabled("InsertImg")){
					if(!file.poster)
						return;
		        	if(!editor.getParagraph() && !editor.isInTable())
		        		editor.execCmd("FormatBlock", "p");
					var el = editor.createImg(file.poster, file.type||"intfile")
					AFW.RTF.setURLInfo(el, {type:file.type||"intfile", key:file.key||file.name})
					el.alt = el.title=file.title || file.name;
					editor.getRange().setStartAfter(el);
				}
			}
			
			function handleFile(file, cmd){
				
				var filename = file.name.toLowerCase();
				
				if(cmd=="image"){
		        	// create image
		        	
		        	if(!editor.getParagraph() && !editor.isInTable())
		        		editor.execCmd("FormatBlock", "p");
		        	
					var el = editor.createImg(CM.url.file + "/images/ajax-loader-bar.gif", "intfile")
					AFW.RTF.setURLInfo(el, {type:"intfile", key:filename})
					el.alt = "uploading ..."
					editor.getRange().setStartAfter(el);
					
					UploadManager.upload(file, {
						callback: function(filename){
					    	AFW.RTF.setURLInfo(el, {type:"intfile", key:filename})
					    	el.src="../../file/" + Sitepanel.getContentInfo().contentkey + "/"+filename;
					    	el.alt = filename;
					    	el.style.opacity=null;
						}
					})
				}
				else{
					// create link
					var el = editor.createLink(filename, filename, "intfile");
					AFW.RTF.setURLInfo(el, {type:"intfile", key:filename})
					editor.getRange().setStartAfter(el);
				}
			}
			
			var dropbox = editor.editelement;
			
		    dropbox.addEventListener("dragenter", dragenter, true);
		    dropbox.addEventListener("dragover", dragover, true);
		    dropbox.addEventListener("drop", drop, true);

		    editor.focus();
			//window.editor=editor	// dbg
			return editor;
		}
		
		function close(){
			Sitepanel.getWindow().onbeforeunload=null;
			editor.closeEditor()
		}

		function focus(){
			editor.focus();
		}
		function update(data, data_encoded){
			editor.setRTFHTML(data, data_encoded)
		}
	
		function getContent(){
			return editor.getRTFHTML()
		}
		
		return {
			toolpanel: "toolpanels/rtf",
			data_type: "rtf",
			open: open,
			close: close,
			update: update,
			focus: focus,
			getContent: getContent
		}
		
	}()

	var textblock_editor = function(){
	
		var editor;
		
		function open(item, option){

			Sitepanel.getWindow().onbeforeunload=function(){
				return "Daten wurden noch nicht gespeichert";
			}

			editor = new AFW.RTF.editor("item_"+item, {
				document: Sitepanel.getDocument(),
				contentinfo: Sitepanel.getContentInfo(),
				width: "100%"
			})
			return editor;
		}
	
		return {
			data_type: "textblock",
			open: open,
			update: function(data, data_encoded){
				//editor.setRTFHTML(data, data)
				editor.setRTFHTML(data, data_encoded)
			},
			close: function(){
				Sitepanel.getWindow().onbeforeunload=null;
				editor.closeEditor()
			},
			focus: function(){
				editor.focus();
			},
			getContent: function(){
				return editor.getRTFHTML()
			}
		}
		
	}();

	var text_editor = function(){
		
		var item_el, edit_el, value_el
		
		function open(item){
			Sitepanel.getWindow().onbeforeunload=function(){
				return "Daten wurden noch nicht gespeichert";
			}
			value_el = Sitepanel.getItem(item).hide();
			item_el = value_el.closest(".WGA-Item");
			if(this.data_type=="textarea"){
				item_el.append($("<textarea>", {
					style: "width:100%;font:inherit"
				}).autogrow())
			}
			else item_el.append($("<input>", {
				"type": "text",
				"class": "text-editor-input",
				"style": "width:" + (this.data_type=="number"||this.data_type=="date" ? "100px" : "100%")
			}))
			var unencoded_val = $(".WGA-Item-Value-Unencoded", item_el).text()
			edit_el = item_el.find("input,textarea")
					.val(unencoded_val || value_el.text())
					.focus();
			return this;
		}
		function close(){
			Sitepanel.getWindow().onbeforeunload=null;
			edit_el.remove();
			value_el.show();
		}
		function focus(){
			edit_el.focus();
		}
		function setContent(data){
			edit_el.val(data);
		}
		function update(data_unencoded, data_encoded){
			value_el.html(data_encoded||data_unencoded);	// data is html encoded
			$(".WGA-Item-Value-Unencoded", item_el).text(data_unencoded)
		}
		function getContent(){
			return edit_el.val();
		}
		
		return {
			data_type: "text",
			open: open,
			close: close,
			update: update,
			focus: focus,
			getContent: getContent,
			setContent: setContent		// used by toolbar for some editors
		}
	}();
	
	var date_editor = function(){
		return $.extend({}, text_editor, {
			toolpanel: "toolpanels/date",
			data_type: "date"
		})	
	}()

	var number_editor = function(){
		return $.extend({}, text_editor, {
			data_type: "number"		
		})	
	}()

	var textarea_editor = function(){
		return $.extend({}, text_editor, {
			data_type: "textarea"		
		})	
	}()

	var custom_editor = function(){
		var item_el, value_el
		return {
			open: function(item){
				Sitepanel.getWindow().onbeforeunload=function(){
					return "Daten wurden noch nicht gespeichert";
				}
				value_el = Sitepanel.getItem(item).hide();
				item_el = value_el.closest(".WGA-Item");
				$(".WGA-Custom-Form", item_el).show();
				return this;
			},
			close: function(){
				Sitepanel.getWindow().onbeforeunload=null;
				$(".WGA-Custom-Form", item_el).hide();
				value_el.show();
			},
			getContent: function(){
				return ""
			},
			focus: function(){
				$(".WGA-Custom-Form form input", item_el).first().focus();
			},
			update: function(){
				var form = $(".WGA-Custom-Form form", item_el)
				form.submit()
			}
		}
	}()

	// public interface
	return {
	
		rtf: rtf_editor,
		textblock: textblock_editor,
		text: text_editor,
		textarea: textarea_editor,
		number: number_editor,
		date: date_editor,
		custom: custom_editor
	
	}

})