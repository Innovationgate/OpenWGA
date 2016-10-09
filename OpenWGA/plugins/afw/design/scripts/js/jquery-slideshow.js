/*
 *	jquery-plugin slideshow
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define("jquery-slideshow", ['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	$.fn.slideshow = function(config){
		var config = config||{};
		return this.each(function(){

			var $this = $(this),
			 	pause_time = config.pause || $this.data("pause") || 3000,
				fade_time = config.fade || $this.data("fade") || 5000,
				repeat = config.repeat || $this.data("repeat"),
				effect = config.effect || $this.data("effect") || "fadeIn",
				pages = $this.find(".slideshow-page"),
				height=0,
				i=0;
			
			$this.css({
				position: "relative"
			});
			pages.each(function(){
				$(this).css("maxWidth", "100%");
				var h = $(this).height();
				var w = $(this).width();
				$(this).css({
					position: "absolute",
					display: "none",
					top: "50%",
					left: "50%",
					marginLeft: -w/2,
					marginTop: -h/2
				})
				if(h>height)
					height=h
			})
			$this.height(height);
			
			pages.eq(i).show()
			setTimeout(slideshow, pages.eq(i).data("pause") || pause_time)
			
			function next(index){
				i=index;
				var pause = pages.eq(i).data("pause") || pause_time; 
				pages.eq(i)[effect](fade_time, function(){
					setTimeout(slideshow, pause)
				})				
			}
			
			function slideshow(){
				if(i<pages.length-1){
					pages.eq(i).fadeOut(fade_time)
					next(++i);
				}
				else if(repeat){
					pages.eq(i).fadeOut(fade_time)
					next(0);					
				}
			}
		})
	}
	
});
