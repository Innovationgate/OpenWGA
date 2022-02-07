define(["jquery"], function($){

	var context,
		pageRenderedListener,
		contextChangeListener;
	
	WGA.event.addListener("appnav", "content-changed", function(ev){
		context = ev.params;
		contextChangeListener && contextChangeListener(context);
		$("#appnav").show();
	})

	WGA.event.addListener("appnav", "page-rendered", function(ev){
		pageRenderedListener && pageRenderedListener(ev);
	})

	$(document).on("click.toolbar", "#appnav .toolbar [data-view]", function(ev){

		ev.preventDefault();
		$this = $(this)

		if($this.hasClass("selected")){
			$this.removeClass("selected");
			$("#page").removeClass("appnav")
		}
		else {			
			$("#page").addClass("appnav")
			WGA.event.fireEvent("appnav-view-change", "appnav", {
				view: $this.data("view")
			})
		}
		
	})
	
	return {
		
		getContext: function(){
			return context;
		},
		setContextChangeListener: function(listener){
			contextChangeListener = listener;
		},
		setPageRenderedListener: function(listener){
			pageRenderedListener = listener;
		},
		selectView: function(view){
			$("#appnav .toolbar [data-view]").removeClass("selected")
			$("#appnav .toolbar [data-view=" + view + "]").addClass("selected");
		}
		
	}
	
})