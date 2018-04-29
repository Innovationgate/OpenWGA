/*
 *	jquery-plugin wga_tree
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define("jquery-tree", ['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	function loadJson(el, callback){

		var url = el.data("url") || el.parents(".wga_tree").first().data("url");
		if(!url){
			if(callback)
				callback(el)
			return;
		}
		
		$.ajax({
			url: url,
			data: {
				id: el.data("id")||"root"
			},
			dataType: "json",
			context: {
				callback: callback
			},
			success: function(data, status){
				addNodes(el, data || []);
				if(this.callback)
					this.callback.call(el);
			},
			error: function(xhr, status, error){
				console.log(xhr)
				alert("jquery-tree load error: " + status);
			}
		})
	}

	function toggleTwisty(ev){
		ev.stopPropagation();
		var node = $(this).parents('.node').first();
		if(node.hasClass('collapsed'))
			expandNode(node)
		else collapseNode(node)
	}
	
	function selectEntry(ev){
		ev.preventDefault();
		var node = $(this).parents('.node').first()
		selectNode(node, true);
	}

	function addNode(el, data, select){
		
		var level = el.data("level")||0
		
		var ul = el.find("ul").first()
		if(!ul.length){
			ul = $("<ul/>");
			el.append(ul);
		}
		el.attr("data-haschildren", "true");

		var hasChildren = data.hasChildren||false;
		if(data.children)
			hasChildren = (data.children.length>0)
		
		var li = $("<li/>", {
			"class": "node collapsed",
			"data-id": data.id,
			"data-href": data.href,
			"data-level": level+1,
			"data-title": data.title,
			"data-context": data.context,
			"data-haschildren": hasChildren
		})
		ul.append(li);

		var entry_class = "clearfix entry"
		if(data.cssclass)
			entry_class += " " + data.cssclass 
		var entry = $("<div/>", {
			"class": entry_class
		})
		li.append(entry);

		var indent = $("<span/>", {
			"class": "indent",
			html: "&nbsp;",
			style: "margin-left:"+(level-1)*20+"px"
		})
		entry.append(indent);

		var twisty = $("<span/>", {
			"class": "twisty",
			html: "&nbsp;"
		}).on("click", toggleTwisty)
		entry.append(twisty);

		var icon = $("<span/>", {
			"class": "icon",
			html: "&nbsp;"
		})
		if(data.iconurl)
			icon.css("background-image", "url('"+data.iconurl+"')");

		var link = $("<a/>", {
			href: data.href
		}).on({
			"click": selectEntry,
			"dblclick": function(){
				var node = $(this)
				node.trigger("editnode", {
					node: node,
					id: node.data("id"),
					href: node.data("href"),
					level: node.data("level"),
					title: node.data("title"),
					context: node.data("context"),
					haschildren: node.data("haschildren")
				})
			}
		})
		
		link.append(icon)
		link.attr("draggable", true);
		
		var symbol = $("<div/>", {
			"class": "symbol " + (data.symbolclass||"")
		})
		link.append(symbol);			
		
		var link_text = $("<div/>", {
			"class": "link-text",
			html: data.html || data.title
		})
		link.append(link_text);
				
		entry.append(link)

		if(data.children)
			addNodes(li, data.children);

		if(select){
			selectNode(li, true);
		}

	}

	function addNodes(el, data){
		el.find("ul").remove()
		for(var i=0; i<data.length; i++){
			addNode(el, data[i]);
		}
	}

	function collapseNode(node){
		node.addClass("collapsed")
	}

	function expandNode(node, callback){
		if(node.find("ul").length==0){
			// need load data
			node.addClass("loading");
			loadJson(node, function(){
				node.removeClass("loading");
				setTimeout(function(){
					node.removeClass("collapsed")
					if(callback)
						callback(node)
				}, 10);
			})
		}
		else{
			node.removeClass("collapsed");
			if(callback)
				callback(node)
		}
	}

	function selectNode(node, trigger_selected){
		node.parents(".wga_tree").first().find(".node").removeClass("selected");
		node.addClass("selected")
		node.parents(".node").removeClass("collapsed");
		setTimeout(function(){
			scrollIntoView(node.children("div").first())
		}, 200);
		if(trigger_selected){
			node.trigger("select", {
				node: node,
				id: node.data("id"),
				href: node.data("href"),
				level: node.data("level"),
				title: node.data("title"),
				context: node.data("context"),
				haschildren: node.data("haschildren")
			})
		}
	}

	function scrollIntoView(element) {
		var container = element.offsetParent();
		
		var containerScrollTop = container.scrollTop();
		var containerHeight = container.height();
		
		var elemTop = element.position().top;
		var elemBottom = elemTop + element.height();

		if(elemTop<0){
			var top = containerScrollTop + elemTop
			if(top<containerHeight)
				top=0;
			container.animate({scrollTop: top}, 100)
		}
		else if (elemBottom > containerHeight){
			container.animate({scrollTop: containerScrollTop + elemBottom - containerHeight}, 100)
		}
	}
	
	function selectpath(path, parentNode){
		var parts = path.split("/");
		var node = parentNode || this;
		var id = parts[0];
				
		node.find(".node").removeClass("selected");
		
		if(node.find("ul").length==0){
			// not loaded yet? expand and try again:
			expandNode(node, function(expandedNode){
				// we should find id now:
				var node = $(".node[data-id='"+id+"']", expandedNode);
				if(node.length)
					selectpath(path, expandedNode)	// try again
				// else not found.
			})
			return;
		}
				
		var node = $(".node[data-id='"+id+"']", node);
		if(!node.length){
			//console.log("not found", id);
			return;		// not found
		}	
		
		parts.shift()
		if(parts.length==0){
			// last path element: just select it:
			selectNode(node)
		}
		else selectpath(parts.join("/"), node)
	}

	function reload(config){
		if(config.url)
			this.data("url", config.url)
		if(config.rootId)
			this.data("id", config.rootId)
		this.html('<ul><li class="loading" style="margin-left:20px">Loading ...</li></ul>');
		loadJson(this, function(){
			if(config.selectpath){
				selectpath(config.selectpath, this);
			}
		});
	}

	function recalcLevels(node){
		var level = node.parentsUntil(".wga_tree", "li").length+1
		node.data("level", level);
		var span = node.find("> .entry .indent").first();
		span.css("margin-left", (level-2)*20);
		node.find("li").each(function(){
			recalcLevels($(this))
		})
	}

	function handleDragStart(e){
		console.log("handleDragStart", this, e.dataTransfer, e);
		if(e.originalEvent.type=="mousedown")
			e.preventDefault();
	}

	function getSelectedNode(){
		return this.find("li.selected")
	}

	function reloadSelectedNode(){
		var selected_node = this.find(".node.selected")
		var collapsed = selected_node.hasClass("collapsed");
		var selected_id = selected_node.data("id");
		var node = selected_node.parents(".node").first();
		if(node.length){
			node.find("ul").remove();
			expandNode(node, function(node){
				var node = node.find("[data-id="+selected_id + "]")
				selectNode(node);
				if(!collapsed)
					expandNode(node);
			});
		}
	}

	function findNode(root, node_or_id){		
		if(typeof(node_or_id)=="string")
			return root.find("[data-id='" + node_or_id + "']")
		else return $(node_or_id);
	}

	function updateNode(node, data){
		if(typeof(data.cssclass)=="string")
			$("> .entry", node).attr("class", "clearfix entry " + data.cssclass)
		if(typeof(data.symbolclass)=="string")
			$("> .entry .symbol", node).attr("class", "symbol " + data.symbolclass)
		if(data.html||data.title)
			$("> .entry .link-text", node).html(data.html||data.title)
		if(data.iconurl)
			$("> .entry .icon", node).css("background-image", "url('"+data.iconurl+"')")
		if(data.title)
			node.data("title", data.title)
		if(data.context)
			node.data("context", data.context)
	}
	
	function removeNode(node, select_parent){
		var parent = node.parents('.node').first();
		node.remove();
		var children = parent.find(".node")
		if(!children.length){
			parent.attr("data-haschildren", false)
			collapseNode(parent);
		}
		if(select_parent)
			selectNode(parent, true);
	}

	var exports={
		selectpath: 	selectpath,
		reloadselected: reloadSelectedNode,
		
		addnode: function(node_or_id, data, select){
			return addNode(findNode(this, node_or_id), data, select)
		},
		addnodes: function(node_or_id, data){
			return addNodes(findNode(this, node_or_id), data)
		},
		expandnode: function(node_or_id, callback){
			return expandNode(findNode(this, node_or_id), callback)
		},
		collapsenode: function(node_or_id){
			return collapseNode(findNode(this, node_or_id))
		},
		selectnode: function(node_or_id, trigger_selected){
			return selectNode(findNode(this, node_or_id), trigger_selected)
		},

		updatenode: function(node_or_id, data){
			return updateNode(findNode(this, node_or_id), data)
		},
		removenode: function(node_or_id, select_parent){
			return removeNode(findNode(this, node_or_id), select_parent)
		},
		
		reload: reload
	}

	$.fn.wga_tree = function(config){
		var config = config||{};
		var args = [];
		for(i=1; i<arguments.length; i++)
			args.push(arguments[i]);
			
		return this.each(function(){
			$this = $(this);
			if(typeof(config)=="string"){
				try{
					var f = exports[config.toLowerCase()]
					return f.apply($this, args);
				}
				catch(e){
					throw("jquery plugin wga_tree: method " + config + " failed: " + e)
					return null;
				}
			}
			else{
				$this.addClass("wga_tree");
				var url = config.url || $this.data("url");
				if(url){
					$this.data("url", url)
					loadJson($this);
				}
				if(config.data)
					addNodes($this, config.data)
				
				if(!config.dragdrop)
					return;
				
				var placeholder = $('<li class="node placeholder" data-haschildren="false"><div class="entry"><span class="indent">&nbsp;</span><span class="twisty">&nbsp;</span><a>Insert here</a></div></li>');
				var drag_el_id, drag_el;
				var mayDrop;
				var expand_timer=null;
				var drag_img = document.createElement("img")
				drag_img.src="/plugin-wga-app-framework/file/images/wordprocessing.png"
				
				function dragEnd(el){
					drag_el.removeClass("dragging");
					el.removeClass("drop-add-children");
					placeholder.remove();
					drag_el=null;
				}
				
				$this.on({
					dragstart: function(e){
						//console.log("dragstart", e, e.target);
						drag_el = $(e.target).parentsUntil(".wga-tree", "li.node").first();
						if(drag_el.length==0){
							return drag_el=null;
						}						
						drag_el_id = drag_el.data("id");
						drag_el.addClass("dragging");
						mayDrop=true;
						
						//e.originalEvent.dataTransfer.effectAllowed = "copyLink"
						e.originalEvent.dataTransfer.setDragImage(drag_img, 0, 0);
						e.originalEvent.dataTransfer.setData("wga/link", JSON.stringify({
							title: drag_el.data("title"),
							id: drag_el.data("id"),
							context: drag_el.data("context"),
							href: drag_el.data("href")
						}))

					},
					dragover: function(e){
						if(!drag_el)
							return;
						
						e.preventDefault();
						
						var el = $(e.target).parents("li").first()
						var parent = el.parents("li").first()
						
						/*
						console.log("el", el)
						console.log("parent", parent)						
						console.log("is-children", el.parents("li").index(drag_el))
						*/
						if(el.parents("li").index(drag_el)>=0)
							return;		// don't drag to children
						
						if(el.hasClass("placeholder") || el.data("id")==drag_el_id)
							return;

						if(drag_el.parents("li").first().data("id")==el.data("id")){
							el.removeClass("drop-add-children");
							return;
						}
						
						if(el.hasClass("collapsed") && el.data("haschildren") && !expand_timer){
							// expand node
							expand_timer = setTimeout(function(){
								expandNode(el, function(){
									el.removeClass("drop-add-children");
								});
								expand_timer=null;
							}, 1000)
						}

						var pos = el.offset();
						var height = el.find(".entry").first().height();

						placeholder.find(".indent").css("margin-left", el.find(".indent").css("margin-left"))
						mayDrop=true;
						//console.log(e.originalEvent.clientY, pos.top, height)
						if(e.originalEvent.clientY < pos.top+height/3){
							if(config.mayDrop && config.mayDrop(drag_el.data(), parent.data())==false){
								mayDrop=false
								el.removeClass("drop-add-children");
								placeholder.remove()
								return 
							}
							if(el.prev("li").data("id")!=drag_el_id){
								placeholder.insertBefore(el);
								el.removeClass("drop-add-children");
							}
							else placeholder.remove()
						}
						else if(e.originalEvent.clientY > pos.top+height*2/3){
							if(config.mayDrop && config.mayDrop(drag_el.data(), parent.data())==false){
								mayDrop=false
								el.removeClass("drop-add-children");
								placeholder.remove()
								return 
							}
							if(el.next("li").data("id")!=drag_el_id){
								placeholder.insertAfter(el);
								el.removeClass("drop-add-children");
							}
							else placeholder.remove()
						}
						else{
							placeholder.remove()
							if(config.mayDrop && config.mayDrop(drag_el.data(), el.data())==false){
								mayDrop=false
								el.removeClass("drop-add-children");
								return 
							}
							el.addClass("drop-add-children");
						}
					},
					dragenter: function(e){
						e.preventDefault();
						//console.log("dragenter");
					},
					dragleave: function(e){
						e.preventDefault();
						//console.log("dragleave");
						var el = $(e.target).parents("li").first()
						el.removeClass("drop-add-children");
						if(expand_timer)
							clearTimeout(expand_timer)
						expand_timer=null;
					},
					dragend: function(e){
						//console.log("dragend");
						if(!drag_el)
							return;
						drag_el.removeClass("dragging");
						var el = $(e.target).parents("li").first()
						//console.log($(e.target), el);
						el.removeClass("drop-add-children");
						placeholder.remove();
						drag_el=null;

						if(expand_timer)
							clearTimeout(expand_timer)
						expand_timer=null;
					},
					drop: function(e){
						e.preventDefault();

						//console.log("drop", drag_el, $(e.target), $(e.target).parents("li").first(), drag_el, mayDrop);
						if(!drag_el || !mayDrop)
							return;
						
						var dragged_el = drag_el; 	// local copy because dragend will clear drag_el
						
						var el = $(e.target).parents("li").first()
						dragged_el.removeClass("dragging");

						if(el.parents("li").index(drag_el)>=0)
							return;		// don't drag to children

						if(el.data("id")!=dragged_el.data("id")){
							
							var parent = dragged_el.parents("li").first();	// old parent
							
							if(el.hasClass("drop-add-children")){
								
								el.removeClass("drop-add-children");
								var old_level = Number(dragged_el.data("level"));
								
								expandNode(el, function(){
									var children = el.find("ul").first()
									if(children.length==0)
										el.append("<ul></ul>")
									dragged_el.appendTo(el.find("ul").first())
									el.attr("data-haschildren", "true");
									
									recalcLevels(dragged_el)
									
									if(parent.find("li").length==0)
										parent.attr("data-haschildren", "false");
									dragged_el.trigger("moved", {
										id: dragged_el.data("id"),
										href: dragged_el.data("href"),
										parent_id: el.data("id"),
										index: el.find("li:not(.placeholder)").index(dragged_el) 
									})
									
								});
							}
							else {
								dragged_el.insertAfter(placeholder);
								recalcLevels(dragged_el)

								if(parent.find("li").length==0)
									parent.attr("data-haschildren", "false");
								dragged_el.trigger("moved", {
										id: dragged_el.data("id"),
										href: dragged_el.data("href"),
										parent_id: dragged_el.parents("li").first().data("id"),
										index: dragged_el.parents("li").first().find("li:not(.placeholder)").index(dragged_el)
								})
							}

						}

						placeholder.remove();
						
					}
				})
			}
		})
	}
	
});
