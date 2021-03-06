define(["sitepanel", "appnav"], function(SitePanel, Appnav){
	
	var outline_in_doc = false;

	function onContextChange(context){
		updateOutline()
		if(outline_in_doc)
			SitePanel.createStyle($("#heading-helper-css").text(), "heading-helper")		
	}
	
	WGA.event.addListener("*", "CMS_save_item", function(){
		setTimeout(updateOutline, 250)
	})
	
	function init(){
		$("#app-outline input").click(function(){
			showHeadings(this.checked);
		});
		showHeadings(true);
		updateOutline();
		Appnav.setContextChangeListener(onContextChange)
		
		$("#app-outline .struct").on("click", ".tag", function(){
			var index = $(this).data("hx_id");
			var el = $("[data-hx_id=" + index + "]", SitePanel.getDocument());
			el[0] && el[0].scrollIntoView();
		})
		
		Appnav.selectView("outline")
	}
	
	function updateOutline(){
		
		var context = Appnav.getContext();
		if(!context)
			return;
		
		if(context.status=="w"){
			// read page with URL param $clean to get clean HTML without item-editors
		
			$("#app-outline .struct").html('<span class="loading">loading clean HTML...</span>');
			
			var win = SitePanel.getWindow();
			var href;
			
			if(win.location && win.location.href){
				href = win.location.href;
				if(win.location.search)
					href += "&$clean"
				else href += "?$clean"
			}
			else href = SitePanel.iframe().attr("src") + "?$clean"
			
			$.get(href).success(function(html){
				var el = document.createElement("div");
				el.innerHTML = html;
				readHeadings(el)
			})
		}
		else readHeadings($("body", SitePanel.getDocument()))
			
		function readHeadings(el){
			var level, currentLevel, index = 0;
			var errors = false;

			var struct = $("#app-outline .struct")
			var alert = $("#app-outline .alert")
			
			alert.hide().html("");
			struct.html("");
			
			$(el).find("h1, h2, h3, h4, h5, h6").each(function(){
				index++;
				this.dataset.hx_id = index;
				level = Number(this.tagName.substr(1,1));
				if(level > currentLevel+1){
					errors = true;
					for(var i=currentLevel+1; i<level; i++){
						struct.append($("<div/>", {class:"error tag tag-H"+i}).text("fehlt"));
					}
				}
				currentLevel = level
				// be carefull not to add <script>s or other tag>s
				var tag_content = $(this).text().trim();
				if(!tag_content){
					errors=true;
					struct.append($("<div/>", {class:"error tag tag-"+this.tagName}).text("leer"));
				}
				else struct.append($("<div/>", {"data-hx_id": index, class:"tag tag-"+this.tagName}).text(tag_content));
			})

			var h1 = $(el).find("h1").length
			if(h1!=1)
				alert.append("<p>Die Seite sollte genau eine H1 Überschrift enthalten</p>").show();
			if(errors){ 
				alert.append("<p>Die Dokumentstruktur enthält Fehler</p>").show();
			}
			
		}

			
	}
	
	function showHeadings(show){
		outline_in_doc = show
		$("#app-outline input").prop("checked", show);
		if(outline_in_doc)
			SitePanel.createStyle($("#heading-helper-css").text(), "heading-helper")
		else SitePanel.removeStyle("heading-helper")
	}
	
	return {
		init: init,
		showHeadings: showHeadings
	}
})
