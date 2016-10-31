define(function(){

	return {
		
		createStyle: function(css, id){
			var style_id = "cmm-style-"+id;
			if(document.getElementById(style_id))
				return;
		    var head = document.getElementsByTagName("head")[0];
		    var style_el = document.createElement("style");
		    style_el.id=style_id
		    head.appendChild(style_el);
		    
		    if(style_el.styleSheet){// IE
		        style_el.styleSheet.cssText = css;
		    }
		    else {// w3c 
		        var style_content = document.createTextNode(css)
		        style_el.appendChild(style_content);
		    }
		}
		
	};
	
});
