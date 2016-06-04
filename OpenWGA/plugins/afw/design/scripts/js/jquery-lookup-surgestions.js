/*
 *	Lookup Surgestions jquery plugin
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	function handleInput(key_input, lookup, options){
		var input = lookup.find(".lookup-input");
		var surgestion = lookup.find(".lookup-surgestion");
		var popup = lookup.find(".lookup-popup");
		var val = input.val();
		var timer;
		var mouseover=false;
		var bestValue;

		surgestion.val(val ? "" : options.placeholder);
				
		popup.on({
			click: function(e){
				e.stopPropagation()
				e.preventDefault()
				input.val($(this).html()).focus();
				surgestion.val("");
				key_input.val($(this).data("key")); 
				popup.hide();
			}
		}, "li a")
		popup.on("mouseenter", "li", function(e){
			mouseover=true;
			popup.find('.active').removeClass('active')
		})
		popup.on("mouseleave", "li", function(e){
			mouseover=false;
		})

		surgestion.on("focus", function(){
			input.focus();
		})
		
		input.on({
			"focus": function(){
				val = input.val();
			},
			"keydown keypress": function(e){
				var key = e.keyCode;
				if(key==40||key==38){	// up / down
					e.stopPropagation()
					e.preventDefault()
				}
			},
			keyup: function(e){
				//console.log(e.keyCode);
				if(e.keyCode==39 || e.keyCode==13 /* cursor right||RETURN */){
					if(bestValue){
						input.val(bestValue);
						surgestion.val("");
						val=bestValue;
						bestValue="";
						popup.hide();
						popup.html("");
					}
				}
				else if(e.keyCode==40 /* cursor down */){
					e.stopPropagation()
					e.preventDefault()

					if(mouseover)
						return;
					var next;
					var active = popup.find("li.active");
					if(active.length){
						next = active.next();					
					}
					else next = popup.find("li").first()

					if(next.length){
						active.removeClass("active"); 
						next.addClass("active");
						bestValue=next.find("a").html()
						surgestion.val(val + bestValue.substr(val.length));
						key_input.val(next.find("a").data("key"));
					}
				}
				else if(e.keyCode==38 /* cursor up */){
					e.preventDefault();
					e.stopPropagation();
					if(mouseover)
						return;
					var prev;
					var active = popup.find("li.active").removeClass("active");
					if(active.length){
						prev = active.prev();
						if(prev.length){
							prev.addClass("active");
							bestValue=prev.find("a").html()
							surgestion.val(val + bestValue.substr(val.length));
							key_input.val(prev.find("a").data("key"));
						}
					}
				}
				if(input.val()!=val){
					val = input.val();
					surgestion.val("");
					bestValue="";
					key_input.val("");
					if(!val){
						if(timer)
							clearTimeout(timer);
						popup.hide();
						surgestion.val(options.placeholder);
						surgestion.removeClass("no-match");
						key_input.trigger("match", false);
						//lookup.parents(".control-group").removeClass("error");
						return;
					}
					if(timer)
						clearTimeout(timer)
					timer = window.setTimeout(function(){
						$.ajax({
							url: options.url,
							data: { 
								query: val, 
								max: options.max||5 
							},
							success: function (data) {
					            //console.log("lookup", data);
					            if(data.length){
					            	key_input.val(data[0].value);
					            	bestValue = data[0].value;
					            	surgestion.val(val + bestValue.substr(val.length));
					            	if(data.length>1){
					            		popup.html("");
							            for(var i=0; i<data.length; i++)
							            	popup.append('<li><a href="#" data-key="' + data[i].value + '">' + data[i].label + '</a></li>');
							            popup.show();
					            	}
					            	else popup.hide()
					            	//lookup.parents(".control-group").removeClass("error");
					            	surgestion.removeClass("no-match");
					            	key_input.trigger("match", true);
					            }
					            else {
					            	key_input.val("");
					            	popup.hide()
					            	//lookup.parents(".control-group").addClass("error");
					            	surgestion.addClass("no-match");
					            	key_input.trigger("match", false);
					            	surgestion.val(options.no_match || "not found");
					            }
					        }, 
					        dataType: "json"
						});					
					}, 150);
				}
			},

			blur: function(){
				if(!mouseover){
					if(bestValue){
						input.val(bestValue);
						surgestion.val("");
					}
					popup.hide();
				}
			}

		})
	}

	$.fn.lookup_surgestions = function(options){
		return this.each(function(){
			$this = $(this).css("visibility", "hidden");
			$this.wrap('<div class="lookup-wrapper"></div>');
			//$this.hide();
			var name=$this.attr("name");
			//console.log("lookup", $this, name, options.url);
			var lookup = $(options.lookup||"")
			if(!lookup.length){
				lookup = $('<div class="lookup">'
					+ '<input class="lookup-surgestion" type="text" name="' + name + '_surgestion">'
					+ '<input class="lookup-input" autocomplete="off" type="text" name="' + name + '_display">'
					+ '<ul class="lookup-popup typeahead dropdown-menu"></ul>'
					+ '</div>')
					.insertAfter(this);
			}
			var input = lookup.find(".lookup-input")
			if(!input.val())
				input.val(options.value);
			var popup = lookup.find(".lookup-popup").css({
				top: input.outerHeight()-2/*,
				maxWidth: input.outerWidth(),
				overflow: "hidden"*/
			});
			if(options.focus){
				var el = input.focus()[0];
				el && (el.selectionStart=input.val().length);
			}
			if(options.id)
				input.attr("id", options.id);
			
			handleInput($this, lookup, options);
		})
	}

});
