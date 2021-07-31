define(["jquery-tree", "sitepanel", "cm"], function(Tree, Sitepanel, CM){

	var modules;
	var selected_tree_node;
	var mode;
	var deleted_mods;
	
	function init(mods, content_modules, edit_mode, section, last_selected){

		modules = mods;
		mode = edit_mode;
		selected_tree_node=null;
		deleted_mods=[];
		
		var sections = Sitepanel.getWindow().WGA.CMM.sections
		
		var data = [];
		eval('var mods=' + content_modules);
		for(var s in sections){
			data.push({
				title: sections[s].title || s,
				id: "section."+s,
				context: "root",
				cssclass: "section",
				children: getChildMods(mods[s] || [])
			})
		}
		function getChildMods(mods){
			var child_mods=[];
			for(var i=0; i<mods.length; i++){
				var mod = mods[i];
				child_mods.push({		
					href: "#",
					title: mod.title,
					iconurl: modules[mod.module_id] && modules[mod.module_id].icon || CM.url.file + "/images/error.png",
					cssclass: modules[mod.module_id] ? "" : "error",
					id: mod.id,
					context: mod.module_id,
					//data: mod,
					children: getChildMods(mod.children) 
				})
			}
			return child_mods;
		}
				
		$("#module-tree").wga_tree({
			data: data,
			dragdrop: mode=="edit",
			mayDrop: mayDrop
		}).on({
			select: contentModuleSelected,
			editnode: contentModuleEditSelected,
			moved: contentModuleMoved
		})
		
		if(section)
			$("#module-tree").wga_tree("expandnode", "section."+section, function(node){
				$("#module-tree").wga_tree("selectnode", node, true);
			});
		
		last_selected && $("#module-tree").wga_tree("selectnode", last_selected, true); 
		
		$("#dialog-content-modules").on("click", "[data-button=expand-all]", function(e){
			e.preventDefault();
			if(selected_tree_node){
				selected_tree_node.find(".node").removeClass("collapsed")
				selected_tree_node.removeClass("collapsed")
			}
			else $("#module-tree .node").removeClass("collapsed")
		})
		$("#dialog-content-modules").on("click", "[data-button=collapse-all]", function(e){
			e.preventDefault();
			if(selected_tree_node){
				selected_tree_node.find(".node").addClass("collapsed")
				selected_tree_node.addClass("collapsed")
			}
			else $("#module-tree .node").addClass("collapsed")
		})
		$("#dialog-content-modules").on("click", "[data-button=delete]", deleteModuleClick)
		$("#dialog-content-modules").on("click", "[data-action=save-modules]", saveModulesClick)
		$("#dialog-content-modules").on("click", "[data-button=duplicate]", duplicateModulesClick)
		$("#dialog-content-modules").on("click", ".container-toggle", toggleContainerClick)
		$("#dialog-content-modules").on("click", "[data-module-id]", addModuleClick)
		
		$("#dialog-content-modules").on("click", "[data-button=add]", function(e){
			e.preventDefault();
		})
		
	}
	
	function getAllowedModules(parent){
		var mods=null;
		var module_id = parent.context;
		var parent_module = modules[module_id];
		var allowedchildren = parent_module && parent_module.allowedchildren;
		var allowedModulesArray = true;

		if(parent.node){
			// find root node (section) to get allowed modules for this section
			var root_el = $(parent.node);
			if(root_el.data("level")!="1")
				root_el = root_el.parents("[data-level=1]")
		
			var parts = root_el.data("id").split("section.");
			var section = parts[1];
			var sections = Sitepanel.getWindow().WGA.CMM.sections
			allowedModulesArray = sections[section].allowedModules
		}
		
		if(parent_module && (!parent_module.type || parent_module.type.toLowerCase()!="container"))
			return null
		
		for(var m in modules){
			var module = modules[m];

			if(allowedModulesArray!==true && allowedModulesArray.indexOf(module.id)<0)
				continue;
			
			if(allowedchildren && allowedchildren.indexOf(module.id)<0)
				continue;
			if(module.allowedparents && module.allowedparents.indexOf(module_id)<0){
				//console.log("not allowed", module.id, module.allowedparents, module_id)
				continue;
			}
			
			if(module.singleton){
				var els = $("#module-tree").find("[data-context='" + module.id + "']")
				if(els.length){
					continue;
				}
			}
			
			if(!mods)
				mods={}
			var category = module.category || "_"
			if(!mods[category])
				mods[category]=[]
				
			mods[category].push({
				text:		module.title,
				id:			module.id,
				category:	module.category,
				singleton:	module.singleton, 
				icon:		module.icon
			});
			
		}
		return mods;
	}

	function contentModuleEditSelected(ev){
		//console.log("selected", node, node.id, selected_tree_node);
		
		if(mode=="edit" && selected_tree_node && selected_tree_node.data("id").indexOf("section.")<0){
			var link = selected_tree_node.find(".link-text").first();
			var txt = prompt("Neuen Titel eingeben", selected_tree_node.data("title"))
			if(txt){
				link.html(txt)
				selected_tree_node.data("title", txt);
			}
		}
	}

	
	function contentModuleSelected(ev, node){
		//console.log("selected", node, node.id, selected_tree_node);
		
		if(mode=="edit" && selected_tree_node && selected_tree_node.data("id").indexOf("section.")<0 && selected_tree_node.data("id") == node.id){
			return;
		}
		
		selected_tree_node = node.node;
		
		WGA.event.fireEvent("content-module-selected", "*", {
			module: node,
			config: findModuleConfig(node.context)
		})
		
		var content_modules_list = $("#dialog-content-modules .content-modules-toolbar .content-module-list")
		content_modules_list.html('');
		
		// delete button
		if(Number(node.level)>1)
			$("#dialog-content-modules .content-modules-toolbar button[data-button=delete]").removeClass("disabled")
		else $("#dialog-content-modules .content-modules-toolbar button[data-button=delete]").addClass("disabled")
		
		// duplicate button
		if(Number(node.level)>1)
			$("#dialog-content-modules .content-modules-toolbar button[data-button=duplicate]").removeClass("disabled")
		else $("#dialog-content-modules .content-modules-toolbar button[data-button=duplicate]").addClass("disabled")

		// add button
		var modules = getAllowedModules(node);
		if(modules==null){
			$("#dialog-content-modules .content-modules-toolbar button[data-button=add]").addClass("disabled")
			return;
		}
		else $("#dialog-content-modules .content-modules-toolbar button[data-button=add]").removeClass("disabled")
		
		if(modules._ && modules._.length){
			for(var i=0; i<modules._.length; i++){
				var module = modules._[i];
				content_modules_list.append('<a data-module-id="' + module.id + '">'
						+ '<img src="' + module.icon + '">  '
						+ module.text 
						+ '</a>')
			}
		}

		for(var c in modules){
			if(c=="_")
				continue;
			var c_link = $('<a class="container-toggle">' + c + '</a>')
			content_modules_list.append(c_link)
			if(modules[c].length){
				var s = $('<ul class="collapse module-list">')
				content_modules_list.append(s)
				for(var i=0; i<modules[c].length; i++){
					var module = modules[c][i];
					s.append('<li><a data-module-id="' + module.id + '">'
							+ '<img src="' + module.icon + '">  '
							+ module.text 
							+ '</a>')
				}
			}
		}
	}

	function findModuleConfig(id){
		return id && modules[id];
	}

	function mayDrop(sourcenode, destnode){
		/*console.log(sourcenode, destnode)
		return true;*/
		if(!destnode)
			return false;	// drop to tree root not allowed.
		var modules = getAllowedModules(destnode);
		//console.log(sourcenode, destnode, modules)
		for(var c in modules){
			if(modules[c].length){
				for(var i=0; i<modules[c].length; i++){
					var module = modules[c][i];
					if(sourcenode.context==module.id)
						return true;
				}
			}
		}
		return false;
	}

	function contentModuleMoved(ev, node){
		//console.log("moved", ev, node);
	}

	function toggleContainerClick(e){

		e.preventDefault();
		e.stopPropagation();
		
		$this = $(this);
		$("#dialog-content-modules .dropdown-menu .collapse").collapse("hide");
		$("#dialog-content-modules .dropdown-menu .container-toggle").removeClass("open");		
		$this.next().collapse("show").on("shown.bs.collapse", function(){
			var open = $(this).hasClass("in")
			$(this).prev()[open ? "addClass":"removeClass"]("open")
		})
	}


	function guid() {
		function s4() {
			return Math.floor((1 + Math.random()) * 0x10000)
				.toString(16)
				.substring(1);
		}
		return s4() + "_" + s4() + "_" + s4()
	}

	
	function addModuleClick(e){
		e.preventDefault();
		var id = $(this).data("module-id")
		var mod = modules[id];
		var data = {					
			title: mod.title,
			iconurl: mod.icon,
			id: id + "_" + guid(),
			context: id 
		}
		$("#module-tree").wga_tree("addnode", selected_tree_node, data, true);
	}
	
	function deleteModuleClick(e){
		e.preventDefault();
		if($(this).hasClass("disabled"))
			return;
		addToDeleted(selected_tree_node);
		var parent = $("#module-tree").wga_tree("removenode", selected_tree_node, true);
		
		function addToDeleted(node){
			var id = node.data("id");
			if(deleted_mods.indexOf(id)<0)
				deleted_mods.push(id);
			node.find(">ul >.node").each(function(){
				addToDeleted($(this))
			})			
		}
		
	}
	
	function duplicateModulesClick(e){
		e.preventDefault();
		if($(this).hasClass("disabled"))
			return;
		
		var parent = selected_tree_node.parents(".node").first();
		var data = getNodeData(selected_tree_node)

		$("#module-tree").wga_tree("addnode", parent, data, true);
		WGA.event.fireEvent("modules-duplicated", "module-editor", {mod:data})

		function getNodeData(el){
			var mod = modules[el.data("context")];
			var new_id = mod.id + "_" + guid();
			var children = [];
			el.find(">ul >.node").each(function(){
				children.push(getNodeData($(this)))
			})
			return {					
				title: mod.title,
				iconurl: mod.icon,
				id: new_id,
				duplicated_from: el.data("id"),
				context: mod.id,
				children: children
			}
		}

	}
	
	function saveModulesClick(e){
		
		e.preventDefault();
		var els = $("#module-tree").find(">ul>li")
		var data = {};
		els.each(function(){
			$this = $(this);
			var section = $this.data("id").split(".")[1];
			data[section]=getNodes($this)
		})
		WGA.event.fireEvent("save-modules", "*", {
			selected: selected_tree_node && selected_tree_node.data("id"),
			mods: JSON.stringify(data),
			deleted_mods: JSON.stringify(deleted_mods)
		})
		
		function getNodes(el){
			var els = el.find(">ul>li")
			var data = [];
			els.each(function(){
				var $this = $(this);
				var child_data = $this.data()
				data.push({
					id: child_data.id,
					title: child_data.title,
					module_id: child_data.context,
					children: getNodes($this)
				})
			})
			return data;
		}
				
	}

	
	// public interface:
	return {
		init: init
	}

})
