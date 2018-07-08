define(["jquery"], function($){

	var context,
		contextChangeListener;
	
	WGA.event.addListener("siteexplorer", "content-changed", function(ev){
		context = ev.params;

		$("#appnav [data-view='outline']")[ev.params.contentkey ? "show" : "hide"]()
		$("#appnav [data-view='child-docs']")[ev.params.contentkey ? "show" : "hide"]()
		
		if(!ev.params.contentkey){
			$("#appnav .toolbar [data-view=explorer]")
				.siblings().removeClass("selected")
				.end()
				.addClass("selected")
			
			WGA.event.fireEvent("appnav-view-change", "appnav", {
				view: "explorer"
			})
		}
		contextChangeListener && contextChangeListener(context);
		
		$("#appnav").show();
	})
	
	$(document).on("click.toolbar", "#appnav .toolbar [data-view]", function(ev){

		ev.preventDefault();
		$this = $(this)
		
		$this.siblings().removeClass("selected");
		$this.addClass("selected");
		
		WGA.event.fireEvent("appnav-view-change", "appnav", {
			view: $this.data("view")
		})

	})
	
	return {
		
		getContext: function(){
			return context;
		},
		setContextChangeListener: function(listener){
			contextChangeListener = listener;
		}
		
	}
	
})