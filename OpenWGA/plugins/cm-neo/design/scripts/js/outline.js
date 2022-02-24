define(["sitepanel", "appnav"], function(SitePanel, Appnav){
	
	var outline_in_doc = false;

	WGA.event.addListener("*", "CMS_save_item", function(){
		setTimeout(updateOutline, 250)
	})
	
	function onPageRendered(ev){
		//console.log("outline page-rendered", ev.params.contentkey);
		updateOutline()
		if(outline_in_doc)
			SitePanel.createStyle($("#heading-helper-css").text(), "heading-helper")		
	}
	
	function init(){
		$("#app-outline input").click(function(){
			showHeadings(this.checked);
		});
		showHeadings(true);
		updateOutline();
		Appnav.setPageRenderedListener(onPageRendered);
		
		$("#app-outline .struct").on("click", ".tag", function(){
			var index = $(this).data("hx_id");
			var el = $("[data-hx_id=" + index + "]", SitePanel.getDocument());
			el[0] && el[0].scrollIntoView();
		})
		
		Appnav.selectView("outline")
	}
	
	function analyzeDocument(callback, validate){
		
		var docinfo = SitePanel.getContentInfo();
		if(!docinfo)
			return;
		
		if(!validate)
			validate = validateErrors;
		
		if(docinfo.status=="w"){
			// read page with URL param $clean to get clean HTML without item-editors
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
				validate(el);
			})
		}
		else validate($("body", SitePanel.getDocument()))
		
		function validateErrors(el){
			// default validation: just return errors
			var level, 
				currentLevel=0, 
				errors = 0;

			$(el).find("h1, h2, h3, h4, h5, h6").each(function(){
				level = Number(this.tagName.substr(1,1));
				if(level > currentLevel+1){
					errors++;
				}
				currentLevel = level
				// be carefull not to add <script>s or other tag>s
				var tag_content = $(this).text().trim();
				if(!tag_content){
					errors++;
				}
			})

			var h1 = $(el).find("h1").length
			if(h1>1)
				errors++;
			if(callback)
				callback(errors)
			
		}
		
	}
	
	function updateOutline(){

		var struct = $("#app-outline .struct")
		var alert = $("#app-outline .alert")
		
		alert.hide().html("");
		struct.html("");

		analyzeDocument(null, function(el){
			var level, currentLevel=0, index = 0;
			var errors = false;
			
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
			
		})
			
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
		showHeadings: showHeadings,
		analyzeDocument: analyzeDocument,
		removePageRenderedListener: function(){
			Appnav.setPageRenderedListener(null);
		}
	}
})
