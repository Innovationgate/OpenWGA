define([
	"sitepanel",
	"uploadmanager",
	"cm",
	"afw/rtfeditor",
	"jquery-textarea-autogrow"
], function(Sitepanel, UploadManager, CM, RTFEditor){

	
	function makeScriptlets(html){
		// use browsers dom as HTML parser:
		var el = $("<div></div>").html(html);
		
		$("a", el).each(function(){
			
			var link = this;
			
			var info = RTFEditor.getURLInfo(link);
			var linktype=info.type;
			var wgakey=info.key;

			switch(linktype){
				/*
				 * Don't directly write URL to link.href because FF3 will encode this URL in this case
				 * So we write the URL to an Attribute "wga:href" that will later be removed by a regexp
				 */
				case "int":
					link.setAttribute("wga:href", "{%!contenturl:"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "intname":
					link.setAttribute("wga:href", "{%!namedcontenturl:"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "intfile":
					link.setAttribute("wga:href", "{%!fileurl:"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "layout":
					link.setAttribute("wga:href", "{%!layouturl:"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "scriptlet":
					link.setAttribute("wga:href", "{%!"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "file":
				case "extfile":
					var key = wgakey.split("/");
					var container = key[0];
					var filename = key[1];
					var dbkey = key[2];
					var scriptlet = "{%"
					if(dbkey)
						scriptlet += "(db:"+dbkey+")"
					scriptlet += "!fileurl:"+container+","+filename+"%}"
					link.setAttribute("wga:href", scriptlet);
					link.removeAttribute("href");
					break;
			}
		
		})

		$("img", el).each(function(){
			
			var img = this

			var info = RTFEditor.getURLInfo(img);
			var urltype=info.type;
			var wgakey=info.key;
			
			switch(urltype){
				case "file":
				case "extfile":
					var key = wgakey.split("/");
					var container = key[0];
					var filename = key[1];
					var dbkey = key[2];

					if(img.style){
						if(img.style.width){
							var w = img.style.width.indexOf("px");
							if(w)
								filename += "?width~"+parseInt(img.style.width.substr(0, w));
						}
						else if(img.style.height){
							var h = img.style.height.indexOf("px");
							filename += "?height~"+parseInt(img.style.height.substr(0, h));
						}
					}
					
					var scriptlet = "{%"
					if(dbkey)
						scriptlet += "(db:"+dbkey+")"
					scriptlet += "!imgurl:"+container+","+filename+"%}"
					img.setAttribute("wga:src", scriptlet);
					
					scriptlet = "{%"
					if(dbkey)
						scriptlet += "(db:"+dbkey+")"
					scriptlet += "!srcset:"+container+","+filename+"%}"
					img.setAttribute("wga:srcset", scriptlet);
					
					img.removeAttribute("src");
					img.removeAttribute("srcset");
					break;
				case "intfile":
					if(img.style){
						if(img.style.width){
							var index = img.style.width.indexOf("px");
							if(index)
								wgakey += "?width~"+parseInt(img.style.width.substr(0, index));
						}
						else if(img.style.height){
							var index = img.style.height.indexOf("px");
							wgakey += "?height~"+parseInt(img.style.height.substr(0, index));
						}
					}
					
					img.setAttribute("wga:src", "{%!imgurl:"+wgakey+"%}");
					img.setAttribute("wga:srcset", '{%!srcset:' + wgakey + '%}');
					img.removeAttribute("src");
					img.removeAttribute("srcset");
					break;
				case "exturl":
					img.setAttribute("wga:src", img.src);
					img.removeAttribute("src");
					break;
			}
		})

		htmltext = el.html();
		htmltext = htmltext.replace(/wga:href="([^"]*)"/g, 'href="$1"') 
		htmltext = htmltext.replace(/wga:src="([^"]*)"/g, 'src="$1"')
		htmltext = htmltext.replace(/wga:srcset="([^"]*)"/g, '$1') 

		// CM 1.4: embed new attribute wga:urlinfo in scriptlet 
		htmltext = htmltext.replace(/wga:urlinfo="([^"]*)"/g, '{%!rtfsystem:wga:urlinfo="$1"%}');
		
		//console.log(htmltext)
		return htmltext;
	}
	
	var rtf_editor = function(){
	
		var editor;
		var edit_el;
		var edit_el_unencoded;
		var rtf_edit_el;
	
		function open(item, option){
			
			//console.log("open RTF editor", option);
			Sitepanel.getWindow().onbeforeunload=function(){
				return "Daten wurden noch nicht gespeichert";
			}
			
			edit_el = $("#item_"+item, Sitepanel.getDocument())
			edit_el_unencoded = $("#item_"+item+"_unencoded", Sitepanel.getDocument())
			edit_el.hide();
			
			rtf_edit_el = $("<div>").html(edit_el_unencoded.html() || "<br>")
			rtf_edit_el.insertBefore(edit_el);
			
			editor = RTFEditor.edit(rtf_edit_el)

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
			    if(!editor.toolbar)
			    	return;
				var dt = e.dataTransfer;
				var linkEffect = "link";
				if(dt.effectAllowed.toLowerCase().indexOf("link")<0)
					linkEffect = "move";	// Safari does not know "link" as effect
				if(isLinkDrop(dt))
					dt.dropEffect = linkEffect;				
				else if(!isFilesDrop(dt) && !isDesktopDrop(dt))
					dt.dropEffect = "none"
				else dt.dropEffect = (e.shiftKey||forceCreateLink() ? linkEffect:"copy");
				//console.log("dt.dropEffect", dt.dropEffect, dt.effectAllowed)
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
			    
			    if(!editor.toolbar)
			    	return;
			    
			    if(e.dataTransfer.getData("wga/link")){
			    	var data = JSON.parse(e.dataTransfer.getData("wga/link"))
			    	//console.log("link drop", e.dataTransfer.getData("wga/link"))
					var el = editor.createLink(data.href, data.title, "int");
					editor.setURLInfo(el, {type:"int", key:data.id})
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
							insertAttachment(data[i], e.dataTransfer.dropEffect=="move" || e.dataTransfer.dropEffect=="link");
					} 
					return;
				}
				if(!e.dataTransfer.files || e.dataTransfer.files.length==0){
					return;
				}
			    
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
					editor.setURLInfo(el, {type:file.type||"intfile", key:file.key||file.name})
					editor.getRange().setStartAfter(el);
				}
				else if(!editor.toolbar.isCmdDisabled("InsertImg")){
					if(!file.poster){
						return;
					}
		        	if(!editor.getParagraph() && !editor.isInTable())
		        		editor.execCmd("FormatBlock", "p");
					var el = editor.createImg(file.poster, file.type||"intfile")
					editor.setURLInfo(el, {type:file.type||"intfile", key:file.key||file.name})
					el.alt = file.title || file.name;
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
					editor.setURLInfo(el, {type:"intfile", key:filename})
					el.alt = "uploading ..."
					editor.getRange().setStartAfter(el);
					
					UploadManager.upload(file, {
						callback: function(filename){
					    	editor.setURLInfo(el, {type:"intfile", key:filename})
					    	el.src="../../file/" + Sitepanel.getContentInfo().contentkey + "/"+filename;
					    	el.alt = filename;
						}
					})
				}
				else{
					// create link
					var el = editor.createLink(filename, filename, "intfile");
					editor.setURLInfo(el, {type:"intfile", key:filename})
					editor.getRange().setStartAfter(el);
					UploadManager.upload(file)
				}
			}
			
			var dropbox = editor.editelement;
		    dropbox.addEventListener("dragenter", dragenter, true);
		    dropbox.addEventListener("dragover", dragover, true);
		    dropbox.addEventListener("drop", drop, true);
			
		    editor.focus();
			//window.editor=editor	// dbg
		    
		    if(this.onOpen)
		    	this.onOpen(editor)
		    
			return editor;
		}
		
		function close(){
			Sitepanel.getWindow().onbeforeunload=null;
			editor.close();
			editor=null;
			
			rtf_edit_el.remove();
			edit_el.show();
		}

		function focus(){
			editor.focus();
		}
		function update(data, data_encoded){
			edit_el_unencoded.html(data);
			edit_el.html(data_encoded)
			editor.html(data_encoded);
		}
	
		function getContent(){
			return makeScriptlets(editor.html())
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
		
		return $.extend({}, rtf_editor, {
			data_type: "textblock",
			onOpen: function(e){
				editor = e;
				editor.setDefaultParagraphSeparator("div");
			},
			getContent: function(){
				return editor.html();
			},
			toolpanel: null
		})
		
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
					"class": "text-editor-input",
					"style": "width:100%;"
				}).autogrow())
			}
			else item_el.append($("<input>", {
				"type": "text",
				"class": "text-editor-input",
				"style": "width:" + (this.data_type=="number"||this.data_type=="date" ? "5em" : "100%")
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
				Sitepanel.getWindow().onbeforeunload=null;
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