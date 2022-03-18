define(["cm", "jquery", "editors", "uploadmanager", "sitepanel", "jquery-wga-drophandler", "bootstrap"], function(CM, $, Editors, UploadManager, Sitepanel){
	
	var editor;
	var edit_item;
	var edit_item_format;
	var contentInfo;

	/* Click handler */
	var actions = {
		"reload": Sitepanel.reload,
		"upload-file": function(){
			CM.openDialog("upload-file")
		},
		"edit-file-metas": function(){
			CM.openDialog("attachment-metas", {
				filename: $("#sidepannel-content-attachments .thumb.selected").data("filename")
			})
		},
		"delete-attachments": function(){ 
			var filenames = [];
			$("#sidepannel-content-attachments .thumb.selected").each(function(){
				filenames.push($(this).data("filename"))
			})
			CM.openDialog("delete-attachments", {
				filenames: filenames.join(",")
			})	
		}	
	}

	$(document).on("click.action-panel", "#sidepanel-content [data-action]", function(ev){
		ev.preventDefault();
		var action = $(this).data("action");
		if(actions[action]){
			actions[action]();
		}
	})

	WGA.event.addListener("*", "content-metas-changed", function(ev){
		updateContentProperties(ev.params);
	})

	WGA.event.addListener("*", "page-moved", function(){
		Sitepanel.reload();
	})

	WGA.event.addListener("*", "content-changed", function(ev){
	
		contentInfo = ev.params;
		
		updateContentProperties(ev.params);
		updateContentAttachments(ev.params);
		updateContentVersions(ev.params);
		updateAttachmentSources();
		
		editor=null;
		WGA.event.fireEvent('CMS_cancel_item_edit')
		
	})

	WGA.event.addListener("*", "CMS_item_edit", function(ev){
		$("#panel-set-edit").css("display", "inline-block").find("a:first").tab("show");
		$("#panel-set-content").hide().find("li").removeClass("active");
		var opts={}
		ev.params.options && eval("opts="+ev.params.options)
		editor = Editors[ev.params.editor];
		edit_item = ev.params.item;
		edit_item_format = ev.params.format;
		if(editor){
			if(editor.toolpanel){
				require([editor.toolpanel], function(Toolpanel){
					if(Toolpanel.setOptions)
						Toolpanel.setOptions(opts);
					Toolpanel.setEditor(editor.open(ev.params.item, opts));
				})
			}
			else {				
				var editor_object = editor.open(ev.params.item, opts)
				editor_object.toolbar = null;
			}
		}
		$("#section-edit [data-editor]").hide();
		$("#editor-panel-"+ev.params.editor).show();
	})

	WGA.event.addListener("*", "CMS_cancel_item_edit", function(ev){
		
		$("#panel-set-content").css("display", "inline-block")
		$("#panel-set-edit").hide().find("li").removeClass("active");
		
		if(!$("#sidepanel-content .panel-set li.active").length)
			$("#panel-set-content a:first").tab("show");
		
		$("#section-edit .panel").hide();
		if(editor)
			editor.close();
	})

	WGA.event.addListener("*", "CMS_save_image_item", function(ev){
		var url = CM.url.json + "/save-image-item.int.json"
		var params = $.extend({}, contentInfo, {
			item: ev.params.item,
			filename: ev.params.filename,
			container: ev.params.container,
			title: ev.params.title || "",
			alt: ev.params.alt || "",
			image_dbkey: ev.params.dbkey
		}) 
		//console.log("saving item", params);
		$.ajax({
			method: "POST",
			url: url, 
			data: params,
			dataType: "json"
		}) 
	})

	WGA.event.addListener("*", "CMS_save_item", function(ev){
		if(editor){
			var close_editor = ev.params.close_editor;
			var url = CM.url.json + "/save-item.int.json"
			var params = $.extend({}, contentInfo, {
				item: edit_item,
				format: edit_item_format,
				data_type: editor.data_type,
				value: ev.params.remove_item ? null : editor.getContent()
			}) 
			//console.log("saving item", params);
			$.ajax({
				method: "POST",
				url: url, 
				data: params,
				dataType: "json", 
				success: function(data){
					if(!data.success){
						alert(data.message)
						return;
					}
					editor.update(data.value, data.value_encoded);
					if(close_editor){
						//editor.close();
						WGA.event.fireEvent('CMS_cancel_item_edit')						
						updateContentProperties(contentInfo);
					}
					else {
						editor.focus();
						WGA.event.fireEvent('CMS_item_saved')						
					}
				},
				error: function(xhr, status){
					console.log("unable to save item: ", status, xhr);
				}
			})
		}
	})

	WGA.event.addListener("*", "attachments-updated", function(ev){
		updateContentAttachments()
	})

	function updateContentProperties(context){
		var url = CM.jsonURL("content-properties");
		var template = CM.template("sidepannel-content-properties")
		context = context || template.getContext();
		$.getJSON(url, context, function(result){
			template.render(result, context)
		})
	}

	function updateContentAttachments(context){
		var url = CM.jsonURL("content-attachments");
		var template = CM.template("sidepannel-content-attachments")
		var initial_waitForDerivates=null;		
		var count = 0;
		context = context || template.getContext();
		readAttachments()
		
		function readAttachments(){
			$.getJSON(url, context, function(result){
				template.render(result, context)
				if(context.may_update_content){
					$("#sidepannel-content-attachments .sidebar-toolbar [data-action='upload-file']").removeClass("disabled")
					$("#sidepannel-content-attachments .drop-here").show();
				}
				else $("#sidepannel-content-attachments .drop-here").hide();
				
				if(initial_waitForDerivates==null){
					initial_waitForDerivates=[]
					$("#sidepannel-content-attachments .thumb[data-waiting=true]").each(function(){
						initial_waitForDerivates.push($(this).data("filename"))
					})
				}
				var current_waitForDerivates=[];
				$("#sidepannel-content-attachments .thumb[data-waiting=true]").each(function(){
					current_waitForDerivates.push($(this).data("filename"))
				})

				for(var i=0; i<initial_waitForDerivates.length; i++){
					var filename = initial_waitForDerivates[i];
					if(current_waitForDerivates.indexOf(filename)<0){
						var _wga = Sitepanel.getWindow().WGA
						_wga && _wga.event.fireEvent("derivates-updated", "cm-neo", {
							filename: filename								
						})
						//console.log("fire event derivates-updated", filename)
					}
				}
				
				if(count++<10 && current_waitForDerivates.length){
					//console.log("read again", count, current_waitForDerivates)
					setTimeout(readAttachments, 1000)
				}
			})
		}
	}

	function updateContentVersions(context){
		var url = CM.jsonURL("content-versions");
		var template = CM.template("sidepannel-content-versions")
		context = context || template.getContext();
		$.getJSON(url, context, function(result){
			template.render(result, context)
			$("#sidepannel-content-versions")
				.closest(".panel")[result.versions?"show":"hide"]()
				.find(".panel-heading .badge").html(result.versions)
			if($("#sidepannel-content-versions [data-type=archive] a.selected").length)
				return;
			var els = $("#sidepannel-content-versions [data-type=archive] a[data-wgakey]")
				.slice(3)
				.hide();
			if(els.length)
				$("#sidepannel-content-versions [data-type=archive] a.show-all").html("weitere " + els.length + " anzeigen ...")
					.css("display", "block")
					.click(function(ev){
						ev.preventDefault();
						$("#sidepannel-content-versions [data-type=archive] a").show();
						$(this).hide()
					})
			else $("#sidepannel-content-versions [data-type=archive] a.show-all").hide()
		})
	}

	/*
	 * image lib 
	 */
	function updateAttachmentSources(context){
		// read and render img-lib sources
		var url = CM.jsonURL("imglib-attachments");
		var template = CM.template("sidepannel-attachment-source")
		$.getJSON(url, context, function(result){
			template.render(result, context)
			if(result.context){
				template = CM.template("sidepannel-content-attachments")
				template.render(result, context)
				$("#sidepannel-content-attachments .sidebar-toolbar").hide()
			}
			else if(contentInfo){
				$("#sidepannel-content-attachments .sidebar-toolbar").show()
				updateContentAttachments(contentInfo)
			}
		})
	}
	
	function initDomEvents(){
		
		/*
		 * Image Library Event Handler
		 */

		$("#sidepannel-attachment-source").on("click", ".dropdown-menu a", function(ev){
			ev.preventDefault();
			var $this = $(this);
			updateAttachmentSources({
				dbkey: $this.data("dbkey"),
				contentkey: $this.data("contentkey")
			})
		})
	
		/*
		 * Content Attachment event handler
		 */	
		
		$("#sidepanel-content").wga_drophandler({
			onDesktopDrop: function(files){
				if(contentInfo.may_update_content){
					$("#sidepanel-content a[href='#section-content-attachments']").tab("show");
					for(var i=0; i<files.length; i++){
						UploadManager.upload(files[i])
					}
				}
			}
		})
		$("#sidepannel-content-attachments").on({
			click: function(e){
				var panel = $("#sidepannel-content-attachments");
				var el = $(this);
				if(e.metaKey){
					el.toggleClass("selected")
				}
				else{
					panel.find(".thumb").removeClass("selected");	// de-select all
					el.addClass("selected")
				}
				var method = panel.find(".thumb.selected").length ? "removeClass":"addClass";
				panel.find(".sidebar-toolbar [data-action=edit-file-metas]")[method]("disabled")
				if(contentInfo.may_update_content){
					panel.find(".sidebar-toolbar [data-action=delete-attachments]")[method]("disabled")
				}
			},
			dblclick: function(e){
				CM.openDialog("attachment-metas", {
					dbkey: $(this).data("dbkey"),
					container: $(this).data("container"),
					type: $(this).data("type"), 
					filename: $(this).data("filename")
				})
			}		
		}, ".thumb")
		.on({
			dragstart: function(e){
				var el = $(e.target);
				var data = [];
				if(el.hasClass("selected")){
					$(this).find(".thumb.selected").each(function(){
						var $this = $(this) 
						data.push({
							type: $this.data("type") || "intfile",
							key: $this.data("key"),
							dbkey: $this.data("dbkey"),
							container: $this.data("container"),
							name: $this.data("filename"),
							title: $this.data("filetitle"),
							size: $this.data("filesize"),
							url: $this.data("fileurl"),
							poster: $this.data("poster")
						})
					})
				}
				else{	// single img drag 
					data.push({
						type: el.data("type") || "intfile",
						key: el.data("key"),
						dbkey: el.data("dbkey"),
						container: el.data("container"),
						name: el.data("filename"),
						title: el.data("filetitle"),
						size: el.data("filesize"),
						url: el.data("fileurl"),
						poster: el.data("poster")
					})
				}
				try{
					// try-catch bc. IE Edge
					e.originalEvent.dataTransfer.effectAllowed = "copyLink"
					e.originalEvent.dataTransfer.setData("wga/files", JSON.stringify(data))
				}
				catch(e){}
			},
		})
	}
		
	return {
		initDomEvents: initDomEvents
	}
	
})
