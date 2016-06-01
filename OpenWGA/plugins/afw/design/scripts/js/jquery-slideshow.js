/*
 *	jquery-plugin slideshow
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	$.fn.slideshow = function(config){
		var config = config||{};
		return this.each(function(){

			var $this = $(this),
			 	pause_time = config.pause || $this.data("pause") || 3000,
				fade_time = config.fade || $this.data("fade") || 5000,
				images = $this.find("img"),
				height=0,
				i=0;
			
			$this.css({
				position: "relative"
			});
			images.each(function(){
				var h = $(this).height();
				var w = $(this).width();
				$(this).css({
					position: "absolute",
					display: "none",
					maxWidth: "100%",
					top: "50%",
					left: "50%",
					marginLeft: -w/2,
					marginTop: -h/2
				})
				if(h>height)
					height=h
			})
			$this.height(height);
			
			images.eq(0).show()
			setTimeout(slideshow, pause_time)
			
			function slideshow(){
				images.eq(i++).fadeOut(fade_time)
				if(i>=images.length)
					i=0;
				images.eq(i).fadeIn(fade_time, function(){
					setTimeout(slideshow, pause_time)
				})
			}
		})
	}
	
});
