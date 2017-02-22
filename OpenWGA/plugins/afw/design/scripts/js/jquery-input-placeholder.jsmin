/*
 *	jquery-plugin input-placeholder
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	$.fn.placeholder = function(emptyValueText){

		var emptyValueText = emptyValueText||"suchen ..."

		return this.each(function(){
			var el = $(this);
			var value = el.val();

			if("placeholder" in this){
				// Browser supports HTML5 placeholder: just use it.
				this.placeholder=emptyValueText;
				return;
			}

			if(!value){
				el.val(emptyValueText)
				el.addClass("empty");
			}
			
			el.bind({
				keyup: function(ev){
					if(this.value!=value){
						value = this.value;
					}
				},
				blur: function(){
					if(!value){
						el.val(emptyValueText);
						el.addClass("empty");
					}
				},
				focus: function(){
					if(!value){
						el.val("");
						el.removeClass("empty");
					}
				}
			})
			el.parents("form").submit(function(){
				el.val(value);
			})

		})
	}
	
});
