/*
 *	jquery-plugin scrollIntoView
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

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

	$.fn.scrollIntoView = function(){
		var el = $(this).first()
		if(el.length)
			scrollIntoView(el);
	}
	
});