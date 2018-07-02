define(["jquery", "afw/file-uploader"], function($){
	
	var uploads=0;
	var errors=0;
	var uid=0;
	var queue = "#file-upload-queue"
	var upload_url;
		
	function setError(file_id){
		uploads--;
		errors++;
		window.setTimeout(function(){
			errors--;
			$(queue+" [data-id='"+file_id+"']").slideUp(function(){
				$(this).remove();
			  	if(!uploads && !errors)
		  			$(queue).slideUp();
			})
		}, 5000);
	}
	
	return {
		upload: function(file, config){
			config = config||{};
			uploads++;
			uid++;						
			var id = uid;
			$(queue).show().append('<div class="queued-file" data-id="'+id+'">'
						+ '<div class="info">' + file.name + '</div>'
						+ '<div class="progress">'
							+ '<div class="progress-bar">0%</div>'
						+ '</div>'
					+ "</div>");
			
			
			if(config.limit && file.size>config.limit*1000000){
				$(queue+" [data-id='"+id+"'] .progress-bar")
					.addClass("progress-danger")
					.find(".info")
						.addClass("error")
						.html(file.name + " exceeds " + config.limit + "MB");
				setError(id)
				return;
			}
			
			
			upload_url = $(queue).data("url");
			
			new AFW.FileUploader(file, upload_url, {
				
				urlparams: {
					context: $(queue).data("context"),
					filename: file.name
				},
				
				onProgress: function(p){
					var percent = parseInt(100*p);
					$(queue+" [data-id='"+id+"'] .progress-bar").css("width", percent+"%").html(percent+"%"); 
					if(percent==100){
						$(queue+" [data-id='"+id+"'] .progress-bar")
							.addClass("progress-bar-warning")
							.html("processing ...")
					}
				},
				
				onSuccess: function(filename, response){
					window.setTimeout(function(){
						$(queue+" [data-id='"+id+"']").slideUp(function(){
							uploads--
							$(this).remove();
						  	if(!uploads){
						  		WGA.event.fireEvent("attachments-updated", "window", response);
						  		if(!errors)
						  			$(queue).slideUp();
						  	}
						});
					}, 500);
					if(config.callback)
						config.callback(file.name)
				},
				
				onError: function(msg){
					$(queue+" [data-id='"+id+"'] .progress-bar").addClass("progress-bar-danger").removeClass("active");
					$(queue+" [data-id='"+id+"'] .info").addClass("error");		//.html(file.name + ": " + msg);
					setError(id)
				}
				
			})
		}
	}

})
