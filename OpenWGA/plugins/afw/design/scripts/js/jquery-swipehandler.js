/*
 *	jquery-plugin swipehandler
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */
 
!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($) {

	function addSwipeListener(el, config)
	{
		var startX, startY;
		var dx, dy;
		var config = config || {};
		 
		function cancelTouch()
		{
			el.removeEventListener('touchmove', onTouchMove);
			el.removeEventListener('touchend', onTouchEnd);
		}
		 
		function onTouchMove(e)
		{
	   		var _dx = e.touches[0].pageX - startX;
	   		var _dy = e.touches[0].pageY - startY;
	   		if(Math.abs(_dy)<Math.abs(_dx)){
	   			dx = _dx;
	   			dy = _dy;
				e.preventDefault();
				if(config.touchMove)
					config.touchMove(dx);
			}
		}
		 
		function onTouchEnd(e)
		{
		  	cancelTouch();
	  		if(config.touchEnd)
	   			config.touchEnd(dx);
		}
	
		function onTouchStart(e)
		{
		  	if (e.touches.length == 1){
		   		startX = e.touches[0].pageX;
		   		startY = e.touches[0].pageY;
		   		dx=dy=0;
		   		el.addEventListener('touchmove', onTouchMove, false);
		   		el.addEventListener('touchend', onTouchEnd, false);
		   		if(config.touchStart)
		   			config.touchStart();
		  	}
		}
		 
		el.addEventListener && el.addEventListener('touchstart', onTouchStart, false);
	}

	$.fn.swipehandler = function(config){
		return this.each(function(){
			addSwipeListener(this, config)
		})
	}
	
});

