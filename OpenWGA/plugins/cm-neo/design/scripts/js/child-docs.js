define(["cm", "appnav"], function(CM, Appnav){

	var url = CM.url.json + "/child-docs.int.json"
	var template;

	function onContextChange(context){
		context && template && $.getJSON(url, context, function(result){
			template.render(result)
			$("#app-child-docs [data-action=cancel]").hide();
			$("#app-child-docs [data-action=delete]").hide();
			$("#app-child-docs [data-action=edit]").show()
				.prop("disabled", result.children.length==0)
		})		
	}
	
	function init(){
	
		template = CM.template("appnav-child-docs")
		
		$("#app-child-docs").on("click", "a", function(ev){
			ev.preventDefault();
			$("#site-panel").attr("src", this.href);
		})
		
		onContextChange(Appnav.getContext());
		
		Appnav.setContextChangeListener(onContextChange)
		Appnav.selectView("child-docs");

		// edit & remove UI handling
		function enableDelete(){
			var checked = $("#app-child-docs input:checked")
			if(checked.length)
				$("#app-child-docs [data-action=delete]").prop("disabled", false)
			else $("#app-child-docs [data-action=delete]").prop("disabled", true)			
		}
		
		$("#app-child-docs [data-action=edit]").click(function(){
			$("#app-child-docs input").show()
			$("#app-child-docs [data-action=edit]").hide()
			$("#app-child-docs [data-action=cancel]").show();
			$("#app-child-docs [data-action=delete]").show()
			enableDelete()
		})
		$("#app-child-docs [data-action=cancel]").click(function(){
			$("#app-child-docs input").hide()
			$("#app-child-docs [data-action=cancel]").hide();
			$("#app-child-docs [data-action=delete]").hide();
			$("#app-child-docs [data-action=edit]").show()
		})
		$("#app-child-docs").on("click", "input", enableDelete)

		$("#app-child-docs [data-action=delete]").click(function(){
			var checked = $("#app-child-docs input:checked").map(function(){
				return this.value;
			}).get();
			CM.openDialog("delete-pages", {
				pages: checked.join()
			})
		})
		
	}
	
	return {
		init: init
	}
	
})
	
