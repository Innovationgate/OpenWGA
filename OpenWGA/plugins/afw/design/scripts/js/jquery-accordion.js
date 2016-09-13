/*
 *	jquery-plugin accordion
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */
 
!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define([
    	    "jquery",
    	    "css!/plugin-wga-app-framework/css/jquery-accordion"
    	], factory);
  	else factory(root.jQuery);
}(window, function($){

	function expand(panel, speed){
		panel.addClass("active").next().slideDown({
			duration: speed,
			complete: function(){
				panel.trigger("activated", this);
			}
		});
	}

	function activate(el, index){
		var header = el.find(".accordion-header").eq(index)
		var isActive = header.hasClass("active");
		var speed = el.data("accordion-effect-speed") || "fast"
		el.find(".accordion-header").removeClass("active").next().slideUp(speed);
		if(!isActive)
			expand(header, speed)
		
	}
	
	var exports={
		activate: activate
	}	
	
	$.fn.wga_accordion = function(config){
		var config = config||{};
		return this.each(function(){
			var el = $(this);

			if(typeof(config)=="string"){
				try{
					var f = exports[config.toLowerCase()]
					return f.apply(el, [el].concat(args));
				}
				catch(e){
					throw("jquery plugin wga_accordion: method '" + config + "' not found.")
					return null;
				}
			}
			else{
				var speed = config.effectSpeed||'fast';
				el.data("accordion-effect-speed", speed)
				if(config.active!=undefined){
					if(config.delay){
						setTimeout(function(){
							expand(el.find(".accordion-header").eq(config.active), speed)
						}, config.delay);
					}
					else expand(el.find(".accordion-header").eq(config.active), speed)
				}
				el.find(".accordion-header").click(function(ev){
					ev.preventDefault();
					var $this = $(this);
					var isActive = $this.hasClass("active");
					el.find(".accordion-header").removeClass("active").next().slideUp(speed);
					if(!isActive)
						expand($this, speed);
				})
			}
		})
	}

	// data interface:
	$(document).on('click.wga_accordion_activate', "[data-accordion='activate']", function(e){
		e.preventDefault();
		var root_el = $(this).data("target") || $(this).parents(".accordion")
		var index = $(this).data("index") || this.hash.substr(1) || 0
		activate(root_el, index);
	})
	
	
})