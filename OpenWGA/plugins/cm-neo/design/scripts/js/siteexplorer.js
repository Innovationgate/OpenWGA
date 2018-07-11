define(["sitepanel", "jquery", "appnav", "jquery-tree"], function(Sitepanel, $, Appnav){

	var POSITIONING_EVERYWHERE = "all",
  		POSITIONING_FIXEDPARENTS = "fixParentDocs",
  		POSITIONING_FIXEDPARENTTYPES = "fixDocTypes",
  		POSITIONING_ROOTENTRIES = "root",
  		POSITIONING_CHILDENTRIES = "child"
	
	var baseurl;
	var area_json_url;

	var dbkey, area, structkey, language;
	
	function init(explorer_url, areas_url){
		baseurl = explorer_url
		area_json_url = areas_url
		
		$("#explorer").wga_tree({
			dragdrop: true,
			mayDrop: function(source, target){
				
				var sourceStruct = source.context
				var targetStruct = target && target.context

				if(targetStruct && !targetStruct.mayEditChildren)
					return false;		// user is not allowed to create children.
				
				var allowedPositions = sourceStruct.allowedPositions;
				switch(sourceStruct.ctPositioning){
					case POSITIONING_EVERYWHERE:
						return true;
					case POSITIONING_FIXEDPARENTS:
						if(target && allowedPositions.indexOf(target.id)>=0){
							return true;
						}
						break;
					case POSITIONING_FIXEDPARENTTYPES:
						if(targetStruct && allowedPositions.indexOf(targetStruct.contenttype)>=0)
							return true;
						break;
					case POSITIONING_ROOTENTRIES:
						if(!targetStruct)
							return true;
						break;
					case POSITIONING_CHILDENTRIES:
						if(targetStruct)
							return true;
						break;
				}
				return false;
			}
		}).on({
			select: function(ev, node){
				//structkey = node.id;
				//$("#site-panel").attr("src", node.href);
				Sitepanel.reload(node.href);
			},
			moved: function(ev, node){
				//console.log("moved", node);
				WGA.event.fireEvent("move-page", "cm-neo", {
					key: node.id,
					parent: node.parent_id,
					index: node.index,
					after: node.after_id,
					before: node.before_id
				})
			}
		})

		$("#area-dropdown ul").click("li a", function(ev){
			ev.preventDefault();
			var $this = $(ev.target);
			area = $this.html()
			$("#area-dropdown .area-menu .area-title").html(area)
			$("#explorer").wga_tree("reload", {
				url: getURL()
			});
		})

		var context = Appnav.getContext();
		if(context){
			dbkey = context.dbkey;
			area = context.area;
			structkey = context.structkey;
			language = context.language;
			path = context.path;
	
			buildAreas(function(){
				$("#explorer").wga_tree("reload", {
					url: getURL(),
					selectpath: context.path
				});
			})
		}
		
		$("#app-siteexplorer").show();
		Appnav.setContextChangeListener(onContextChange)
		
	}
	
	function getURL(){
		return baseurl + "?dbkey=" + dbkey + "&area=" + area  + "&language=" + language
	}
	
	function buildAreas(callback){
		$.getJSON(area_json_url + "?dbkey=" + dbkey, function(areas){
			//console.log(areas);
			var el = $("#area-dropdown ul")
			el.html("");
			for(var i=0; i<areas.length; i++){
				el.append("<li><a href='#'>" + areas[i] + "</a></li>"); 
			}
			
			if(!area){
				$("#area-dropdown .area-menu .area-title").html("Kein Bereich ausgewählt");
			}
			else $("#area-dropdown .area-menu .area-title").html(area);
			
			if(callback)
				callback()
		})
	}

	function onContextChange(context){
		
		var path = context.path;
		//console.log("siteexplorer content changed event", context);
		if(context.dbkey!=dbkey){
			dbkey = context.dbkey
			area = context.area;
			language = context.language;
			structkey = context.structkey
			//console.log("db changed", dbkey, area, structkey)
			buildAreas(function(){
				$("#explorer").wga_tree("reload", {
					url: getURL(),
					selectpath: path
				});
			})
		}
		else if(context.area!=area || (context.language && context.language!=language)){
			area = context.area;
			structkey = context.structkey
			language = context.language;
			//console.log("area", area, structkey, language)
			$("#explorer").wga_tree("reload", {
				url: getURL(),
				selectpath: path
			});
			$("#area-dropdown .area-menu .area-title").html(area)
		}
		else if(context.structkey!=structkey){
			//console.log("struct changed", context.structkey, structkey);
			structkey = context.structkey
			$("#explorer").wga_tree("selectpath", path);
			//console.log("struct changed -> select path", path);
		}
		else{
			//console.log("nothing to do");
			//$("#explorer").wga_tree("reloadselected");
		}
	}
	
	WGA.event.addListener("siteexplorer", "struct-updated", function(ev){
		var id = ev.params.id;
		$("#explorer").wga_tree("updatenode", id, ev.params);
	})

	WGA.event.addListener("siteexplorer", "page-deleted", function(ev){
		var id = ev.params.id;
		$("#explorer").wga_tree("removenode", id);
	})

	return{
		init: init,
		forceReload: function(){
			area=null;
		}
	}
	
})
