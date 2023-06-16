define(["jquery", "cm", "select2"], function($, CM){

	return function(){

		$("#form-settings textarea").autogrow()
		
		function showLength(el){
			var input_el = $("#form-settings [name=" + el + "]");
			if(!input_el.length)
				return;
			var length_el = input_el.siblings(".input-length").find("span"); 
			input_el.keyup(function(ev){
				length_el.html(this.value.length)
			})
			length_el.html(input_el.val().length)
		}
		showLength("browsertitle");
		showLength("description");
		
		$("#form-settings [name=keywords]").select2({
			tags: true,
			tokenSeparators: [","],
			minimumInputLength: 1,
			placeholder: "- keine -",
			language: {
				inputTooShort: function(e) {
					var t=e.minimum-e.input.length;
					return"Bitte "+t+" Zeichen mehr eingeben"
				}				
			},
			width: "100%",
			ajax: {
				delay: 250,
		        url: CM.url.json +"/keywords.int.json",
		        dataType: 'json',
		        data: function (params) {
		            return {
		                query: params.term, // search term
		                page: params.page||1,
		                pagesize: 25,
		                context: $("#form-settings").data("context"),
		                dbkey: $("#form-settings").data("dbkey")	// used for resource-access-filter
		            };
		        }
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
				$("#form-settings [name=validfrom]").datepicker("option", "maxDate", txt)
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
				var input = $(this).parent().find("input")
				input.val("");
				if(input.prop("name")=="validfrom"){
					$("#form-settings [name=validto]").datepicker("option", "minDate", 0)
				}
				else if(input.prop("name")=="validto"){
					$("#form-settings [name=validfrom]").datepicker("option", "maxDate", "")
				}
				$(this).parent().removeClass("col-sm-8")
					.css("padding", "0")
					.next().hide();
			})
		})
		
		
		$("#form-settings .content").css("visibility", "visible")
		
	}
	
})