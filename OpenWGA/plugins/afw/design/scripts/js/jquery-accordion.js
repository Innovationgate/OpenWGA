/*
 *	jquery-plugin accordion
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */
 
!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(["jquery"], factory);
  	else factory(root.jQuery);
}(window, function($){

	function expand(panel){
		panel.addClass("active").next().slideDown({
			duration: "fast",
			complete: function(){
				panel.trigger("activated", this);
			}
		});
	}

	$.fn.wga_accordion = function(config){
		var config = config||{};
		return this.each(function(){
			var el = $(this);
			if(config.active!=undefined){
				expand(el.find(".accordion-header").eq(config.active))
			}
			el.find(".accordion-header").click(function(ev){
				ev.preventDefault();
				var $this = $(this);
				var isActive = $this.hasClass("active");
				el.find(".accordion-header").removeClass("active").next().slideUp("fast");
				if(!isActive)
					expand($this);
			})
		})
	}

})