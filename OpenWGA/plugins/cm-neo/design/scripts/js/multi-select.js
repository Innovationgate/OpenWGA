define([
	"jquery",
], function($){

	/*
	 * private
	 */
	
	function buildButtomText(el){
		el = $(el);
		var config = el.data("config");
		//console.log("buildButtomText", el)
		var selected=[];
		$("a.selected", el).each(function(){
			var title = $(this).html()
			if($(this).data("group-label"))
				title = $(this).data("group-label") + " " + title;
			selected.push(title)
		})
		if(selected.length==0)
			$("button .text", el).html(config.nonSelectedText || "Nichts ausgewählt")
		else if(selected.length<3)
			$("button .text", el).html(selected.join(", "))
		else $("button .text", el).html(selected.length + " " + (config.nSelectedText || "Werte ausgewählt"))		
	}
	
	function buildValue(value){
		var parts = value.split("|");
		return {
			label: parts[0],
			key: parts.length>1 ? parts[1] : parts[0]
		}		
	}
	
	/*
	 * public
	 */
	
	function buildOptions(el, data){
		el = $(el);
		var ul = $("ul", el);
		ul.html("");
		for(let i=0; i<data.length; i++){
			var value = data[i];
			
			if(typeof(value)=="string"){
				if(value=="-")
					ul.append("<li class='divider'></li>");
				else{
					var value = buildValue(value);
					ul.append("<li><a href='#' data-key='" + value.key + "'>" + value.label + "</a></li>");
				}
			}
			else{
				var group = buildValue(value.label);
				ul.append("<li class='group-divider'><a href='#'>" + group.label + "</a></li>");
				for(let j=0; j<value.values.length; j++){
					if(value.values[j]=="-")
						ul.append("<li class='group'>&nbsp;</li>");
					else{
						var groupvalue = buildValue(value.values[j]);
						ul.append("<li class='group'><a href='#' data-key='" + groupvalue.key + "' data-group='" + group.key + "' data-group-label='" + group.label +"'>" + groupvalue.label + "</a></li>");
					}
				}
			}
		}
		
		disable(el, data.length==0);
		return getOptions(el);
	}
	
	function buildSelect(el, config){
		el = $(el);
		el.data("config", config)
		
		el.html("");
		
		el.addClass("btn-group cm-multiselect")
			.append(
				$("<button disabled class='btn btn-default dropdown-toggle " + config.buttonClass + "' data-toggle='dropdown'>")
					.append("<span class='text'>")
					.append("<span class='caret'>")
			)
			.append("<ul class='dropdown-menu'>")
				
		if(config.multiselect)
			el.addClass("multi");
		
		if(config.data)
			buildOptions(el, config.data);
			
		buildButtomText(el)
		
		el.on("click.ms", "a", function(ev){
			ev.preventDefault();
			if(config.multiselect)
				ev.stopPropagation();		// don't close menu on click
			else $("a[data-key]", el).not(this).removeClass("selected");	// deselect all
			
			var key = $(this).data("key");
			var group = $(this).data("group");
			
			if(!key)
				return;		// group divider
			
			if(group){
				$("a[data-group='" + group + "']", el).not(this).removeClass("selected")
			}
			$(this).toggleClass("selected");
			
			buildButtomText(el)
			
			// build options object
			var options={};
			$(el).find("a[data-key]").each(function(){
				$this = $(this);
				var key = $this.data("key");
				options[key]=$this.hasClass("selected")
			})
			
			config.onChange && config.onChange(options);
		})
	}
	
	function disable(el, value){
		if(!getOptions(el).length)
			$(el).find("button").attr("disabled", true);	// don't enable if we have no options'
		else $(el).find("button").attr("disabled", value)
	}

	function select(el, values){
		el = $(el);
		$("a[data-key]", el).each(function(){
			$this = $(this);
			var key = $this.data("key");
			if(values.indexOf(key)>=0)
				$this.addClass("selected")
			else $this.removeClass("selected")
		})
		buildButtomText(el)
	}

	function getOptions(el){
		var options=[];
		el = $(el);
		$("a[data-key]", el).each(function(){
			options.push($(this).data("key"));
		})
		return options;
	}
	
	// public interface
	return {
		buildSelect: buildSelect,
		disable: disable,
		select: select,
		getOptions: getOptions,
		buildOptions: buildOptions
	}
	
})
