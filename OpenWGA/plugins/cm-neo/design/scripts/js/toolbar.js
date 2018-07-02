define(["cm", "sitepanel", "jquery", "outline", "bootstrap"], function(CM, Sitepanel, $, Outline){

	WGA.event.addListener("*", "CMS_item_edit", function(ev){
		$("#toolbars")
			.find("[data-toolbar=content]").hide()
			.end()
			.find("[data-toolbar=user]").hide()
			.end()
			.find("[data-toolbar=edit]").show()
				.find(".item-edit-info").html(ev.params.label || ev.params.item)

		if(ev.params.editor=="custom")
			$("#toolbars [data-action=save-item]").hide()	// save & continue not possible for custom editors!
		else $("#toolbars [data-action=save-item]").show()
	})

	WGA.event.addListener("*", "CMS_cancel_item_edit", function(ev){
		$("#toolbars")
			.find("[data-toolbar=edit]").hide()
			.end()
			.find("[data-toolbar=content]").show()
			.end()
			.find("[data-toolbar=user]").show()
	})

	WGA.event.addListener("*", "CMS_item_saved", function(ev){
		var button = $("#toolbars [data-action=save-item]")
		button.popover("show");
		setTimeout(function(){
			button.popover("hide");
		}, 2000)
	})	
	
	WGA.event.addListener("*", "clipboard-changed", function(ev){
		$("#toolbars [data-action='clipboard-paste']").parent().removeClass("disabled")
	})
	
	WGA.event.addListener("*", "content-changed", function(ev){
		$("#header .title").hide();
		$("#toolbars")
			
			.show()
			
			.find("[data-action='create-draft']")[ev.params.may_edit_content ? "removeClass":"addClass"]("disabled")
			.end()
			//.find("[data-action='publish-page']")[ev.params.status=='w' ? "removeClass" : "addClass"]("disabled")
			//.end()
			.find("[data-action='content-modules']")[ev.params.status ? "removeClass" : "addClass"]("disabled")
			.end()
			.find("[data-action='seo']")[ev.params.status ? "removeClass" : "addClass"]("disabled")
			.end()

		$("#toolbars [data-action='create-draft']")[ev.params.may_edit_content ? "show" : "hide"]()
		$("#toolbars [data-action='publish-page']")[ev.params.status=='w' ? "show" : "hide"]()
		$("#toolbars [data-action='approve-content']")[ev.params.may_approve_version ? "show" : "hide"]()
		$("#toolbars [data-action='reject-content']")[ev.params.may_approve_version ? "show" : "hide"]()
			
		$("#toolbars [data-action='create-root-page']").parent()[ev.params.may_create_root_page ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action='create-child-page']").parent()[ev.params.may_create_child_page ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action='create-content']").parent()[ev.params.may_create_content ? "removeClass" : "addClass"]("disabled")			
		$("#toolbars [data-action='delete-version']").parent()[ev.params.may_delete_version ? "removeClass" : "addClass"]("disabled")			
		$("#toolbars [data-action='archive-version']").parent()[ev.params.may_archive_version ? "removeClass" : "addClass"]("disabled")			
		$("#toolbars [data-action='delete-page']").parent()[ev.params.may_delete_page ? "removeClass" : "addClass"]("disabled")
		
		$("#toolbars [data-action='settings-content']").parent()[ev.params.contentkey ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action='settings-user-defined']").parent()[ev.params.contentkey ? "removeClass" : "addClass"]("disabled")
			
	})

	/* Click handler */

	var actions = {

		"clipboard-copy": function(){
			var button = $("#tb-button-copy-cb")
			button.popover("show");
			setTimeout(function(){
				button.popover("hide");
			}, 2000)
			WGA.event.fireEvent('clipboard-copy', "*")
		},

		"clipboard-paste": function(){
			CM.openDialog("clipboard-paste");
		},

		"search": function(){
			CM.openDialog("search");
		},

		"settings-content": function(){
			CM.openDialog('settings:content')
		},
		
		"settings-page": function(){
			CM.openDialog('page-settings:render')
		},
		
		"settings-user-defined": function(){
			CM.openDialog('settings:user-defined')
		},

		"approve-content": function(){
			CM.openDialog('approve-content')
		}

		,"reject-content": function(){
			CM.openDialog('reject-content')
		}

		,"create-draft": function(){
			CM.openDialog('create-draft')
		}

		,"create-root-page": function(button){
			CM.openDialog('create-page:root');
		}

		,"create-child-page": function(button){
			CM.openDialog('create-page:child', {
				parent: button.data("parent"),
				pagetype: button.data("pagetype")
			});
		}

		,"create-content": function(button){
			CM.openDialog('create-content');
		}

		,"delete-version": function(button){
			CM.openDialog('delete-version');
		} 
		
		,"archive-version": function(button){
			CM.openDialog('archive-version');
		} 

		,"publish-page": function(){
			CM.openDialog('publish-page');
		}

		,"show-create-areas": function(){
			Sitepanel.showCreateAreas();
			//WGA.event.fireEvent('CMS_showCreateAreas')
		}

		,"save-item-cancel-edit": function(){
			WGA.event.fireEvent('CMS_save_item', "*", {close_editor: true})
		}

		,"userinfo": function(){
			CM.openDialog('userinfo')
		}

		,"save-item": function(button){
			/*button.popover("show");
			setTimeout(function(){
				button.popover("hide");
			}, 2000)*/
			WGA.event.fireEvent('CMS_save_item')
		}

		,"remove-item": function(button){
			button.popover("show");
			setTimeout(function(){
				button.popover("hide");
			}, 2000)
			WGA.event.fireEvent('CMS_save_item', "*", {remove_item: true, close_editor: true})
			//WGA.event.fireEvent('CMS_remove_item')
		}

		,"cancel-edit": function(){
			WGA.event.fireEvent('CMS_cancel_item_edit')
		}

		,"change-user": function(){
			CM.openDialog('change-user');
		},

		"scale": function(button){
			var scale = button.data("scale")
			Sitepanel.scale(scale)
			button.parents(".btn-group").find(".icon").html(scale+"%");
			WGA.event.fireEvent("scale", "*", {scale:scale})
		},

		"toggle-appnav": function(button){
			$('#page').toggleClass('appnav')
			button.html("Panel " + ($('#page').hasClass("appnav") ? "ausblenden" : "einblenden"))
		},

		"view-show-siteexplorer": function(){
			$("#app-outline").hide()
			$("#app-child-docs").hide()
			$("#app-siteexplorer").show()
			Outline.showHeadings(false);
		},
		"view-show-outline": function(){
			$("#app-siteexplorer").hide()
			$("#app-child-docs").hide()
			$("#app-outline").show()
			Outline.showHeadings(true);
		},
		"view-show-child-docs": function(){
			$("#app-siteexplorer").hide()
			$("#app-outline").hide()
			$("#app-child-docs").show()
		},

		"content-modules": function(button){
			CM.openDialog("content-modules", {
				//style: "width:80%;margin-left:-40%;height:80%"
			});
		},

		"delete-page": function(){
			//WGA.event.fireEvent("page-deleted")
			CM.openDialog("delete-page")
		},

		"seo": function(){
			CM.openDialog("seo")
		},

		"logout": function(){
			WGA.event.fireEvent("logout")
		},

		"exit": function(){
			WGA.event.fireEvent("exit")
		}

	};

	$(document).on("click.toolbar", "#toolbars [data-action]", function(ev){

		ev.preventDefault();
		var button = $(ev.target).closest("[data-action]")
		var action = button.data("action");
		if(button.hasClass("disabled") || button.parent().hasClass("disabled"))
			return;
		if(actions[action]){
			actions[action](button);
		}

	})

	// dialogs
	$(document).on("click.toolbar", "#toolbars [data-dialog]", function(ev){
		ev.preventDefault();
		CM.openDialog($(ev.target).closest("[data-dialog]").data("dialog"))
	})

	// popovers
	$("#toolbars [data-action='save-item']").popover({
		placement: "bottom",
		content: "<div style='color:red'><span class='glyphicon glyphicon-ok'></span> Feld wurde gespeichert</div>",
		trigger: "manual",
		html: true
	})
	$("#toolbars [data-action='remove-item']").popover({
		placement: "bottom",
		content: "<div style='color:red'><span class='glyphicon glyphicon-trash'></span> Feld wurde gelöscht</div>",
		trigger: "manual",
		html: true
	})
	$("#tb-button-copy-cb").popover({
		placement: "bottom",
		container: 'body',
		content: "<div style='color:red'><span class='glyphicon glyphicon-ok'></span> Seite wurde in die Ablage eingefügt</div>",
		trigger: "manual",
		html: true
	})

})
