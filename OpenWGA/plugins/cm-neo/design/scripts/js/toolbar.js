define(["cm", "sitepanel", "jquery", "bootstrap"], function(CM, Sitepanel, $){

	var language;
	
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
		$("#toolbars .clipboard-paste-actions").show();
		$("#toolbars .clipboard-page-content").html("Seite '" + ev.params.title + "'");
		
		$("#toolbars [data-action='paste-page").parent()[ev.params.has_released_contents ? "removeClass" : "addClass"]("disabled")
		
		if(ev.params.lang){
			$("#toolbars .clipboard-paste-content-actions").show();
			$("#toolbars .clipboard-content").html("Version " + ev.params.version + " - " + ev.params.lang + " - " + ev.params.status);
		}
		else{
			$("#toolbars .clipboard-paste-content-actions").hide();
		}
	})
	
	WGA.event.addListener("*", "page-rendered", function(ev){
		$("#toolbars [data-action='content-modules']")[ev.params.contentkey && Sitepanel.getWindow().WGA.CMM && Sitepanel.getWindow().WGA.CMM.hasSections ? "removeClass" : "addClass"]("disabled")
	})
	
	WGA.event.addListener("*", "content-changed", function(ev){
		
		language = ev.params.language;
		
		$("#header .title").hide();
		$("#toolbars")
			
			.show()
			
			.find("[data-action='create-draft']")[ev.params.may_edit_content ? "removeClass":"addClass"]("disabled")
			.end()
			.find("[data-action='seo']")[ev.params.status ? "removeClass" : "addClass"]("disabled")
			.end()

		$("#toolbars [data-action='settings-user-defined'] span").html("'"+ev.params.pagetype+"'")
		$("#toolbars [data-action='create-draft']")[ev.params.may_edit_content ? "show" : "hide"]()
		$("#toolbars [data-action='publish-page']")[ev.params.may_publish_page ? "show" : "hide"]()
		$("#toolbars [data-action='approve-content']")[ev.params.may_approve_version ? "show" : "hide"]()
		$("#toolbars [data-action='reject-content']")[ev.params.may_approve_version ? "show" : "hide"]()
			
		$("#toolbars [data-action='create-child-page']").not("[data-parent]").parent()[ev.params.may_create_child_page ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action='create-content']").parent()[ev.params.may_create_content ? "removeClass" : "addClass"]("disabled")			
		$("#toolbars [data-action='delete-version']").parent()[ev.params.may_delete_version ? "removeClass" : "addClass"]("disabled")			
		$("#toolbars [data-action='archive-version']").parent()[ev.params.may_archive_version ? "removeClass" : "addClass"]("disabled")			
		$("#toolbars [data-action='delete-page']").parent()[ev.params.may_delete_page ? "removeClass" : "addClass"]("disabled")
		
		$("#toolbars [data-action='settings-content']").parent()[ev.params.contentkey ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action='settings-user-defined']").parent()[ev.params.contentkey ? "removeClass" : "addClass"]("disabled")
		
		$("#toolbars [data-action-group='settings']")[ev.params.structkey && ev.params.page_visible ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action-group='create']")[ev.params.dbkey ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action-group='delete']")[ev.params.structkey && ev.params.page_visible && !ev.params.is_in_trash ? "removeClass" : "addClass"]("disabled")
		$("#toolbars [data-action-group='clipboard']")[ev.params.structkey && ev.params.page_visible ? "removeClass" : "addClass"]("disabled")
		
		$("#toolbars [data-action='settings-user-defined']").parent()[ev.params.has_userdefined_settings ? "removeClass" : "addClass"]("disabled")
		
		$("#toolbars [data-action='paste-content']").parent()[ev.params.may_update_content ? "removeClass" : "addClass"]("disabled")
		
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

		"paste-page": function(){
			CM.openDialog("paste-page");
		},
		"paste-content": function(){
			CM.openDialog("paste-content");
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
		}

		,"save-item-cancel-edit": function(){
			WGA.event.fireEvent('CMS_save_item', "*", {close_editor: true})
		}

		,"userinfo": function(){
			CM.openDialog('userinfo')
		}

		,"save-item": function(button){
			WGA.event.fireEvent('CMS_save_item')
		}

		,"remove-item": function(button){
			button.popover("show");
			setTimeout(function(){
				button.popover("hide");
			}, 2000)
			WGA.event.fireEvent('CMS_remove_item', "*", {})
		}

		,"cancel-edit": function(){
			WGA.event.fireEvent('CMS_cancel_item_edit')
		}

		,"scale": function(button){
			var scale = button.data("scale")
			Sitepanel.scale(scale)
			button.parents(".btn-group").find(".icon").html(scale+"%");
			WGA.event.fireEvent("scale", "*", {scale:scale})
		}
		
		,"content-modules": function(button){
			CM.openDialog("content-modules");
		}

		,"delete-page": function(){
			CM.openDialog("delete-page", {
				language: language				
			})
		}

		,"preview": function(){
			var url = Sitepanel.getDocument().location.href;
			if(url.indexOf("?")>=0)
				url += "&$clean"
			else url += "?$clean"
			window.open(url, "preview")
		}

		,"seo": function(){
			CM.openDialog("seo")
		}

		,"logout": function(){
			WGA.event.fireEvent("logout")
		}

		,"exit": function(){
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
		content: "<div style='color:red'><span class='fas fa-check'></span> Feld wurde gespeichert</div>",
		trigger: "manual",
		html: true
	})
	$("#toolbars [data-action='remove-item']").popover({
		placement: "bottom",
		content: "<div style='color:red'><span class='far fa-trash-alt'></span> Feld wurde gelöscht</div>",
		trigger: "manual",
		html: true
	})
	$("#tb-button-copy-cb").popover({
		placement: "bottom",
		container: 'body',
		content: "<div style='color:red'><span class='fas fa-check'></span> Seite wurde in die Ablage eingefügt</div>",
		trigger: "manual",
		html: true
	})

})
