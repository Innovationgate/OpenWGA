define(["jquery", "select2"], function($){

	return function(){

		$("#form-settings textarea").autogrow()
		
		$("#form-settings [name=keywords]").select2({
			tags: true,
			tokenSeparators: [",", " "],
			minimumInputLength: 1,
			placeholder: "- keine -",
			language: "de",
			width: "100%",
			ajax: {
				delay: 250,
		        url: "/plugin-cm-neo/json/keywords.de.json",
		        dataType: 'json',
		        data: function (params) {
		            return {
		                query: params.term, // search term
		                page: params.page||1,
		                pagesize: 25,
		                context: $("#form-settings").data("context")
		            };
		        }/*,
		        results: function (data, page) {
		            return { results: data.results};
		        }*/
		    }			
		})
		
		$("#form-settings [name=validfrom]").datepicker({
			minDate: 0,
			onClose: function(txt){
				$("#form-settings [name=validto]").datepicker("option", "minDate", txt||0)
				if(txt){
					$(this).parent().addClass("col-sm-8")
						.css("padding", "0 10px 0 0")
						.next().show()
					$(this).parent().find(".clear-field").show();
				}
				else {
					$(this).parent().removeClass("col-sm-8")
						.css("padding", "0")
						.next().hide();
					$(this).parent().find(".clear-field").hide();
				}
			}
		});
		
		$("#form-settings [name=validto]").datepicker({
			minDate: 0,
			onClose: function(txt){
				$("#form-settings [name=validfrom]").datepicker("option", "maxDate", txt||0)
				if(txt){
					$(this).parent().addClass("col-sm-8")
						.css("padding", "0 10px 0 0")
						.next().show();
					$(this).parent().find(".clear-field").show();
				}
				else {
					$(this).parent().removeClass("col-sm-8")
						.css("padding", "0")
						.next().hide();
					$(this).parent().find(".clear-field").hide();
				}
			}
		})
	
		$("#form-settings .clear-field").each(function(){
			if($(this).parent().find("input").val())
				$(this).show()
			else $(this).hide()
			$(this).click(function(){
				$(this).hide()
				$(this).parent().find("input").val("");
				$(this).parent().removeClass("col-sm-8")
					.css("padding", "0")
					.next().hide();
			})
		})
		
		
		$("#form-settings .content").css("visibility", "visible")
		
	}
	
})