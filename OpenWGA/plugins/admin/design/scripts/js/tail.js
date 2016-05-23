var TAIL = function(){

	var timer;
	var url;
	var index;
	var autoscroll=true;
	var running=false;

	function load(){
		$.getJSON(url, {index:index}, function(data){
			var messages=[]
			index = data.endIndex;
			//console.log(index);
			for(var i=0; i<data.messages.length; i++){
				var msg = data.messages[i];
				var info;
				if(msg.details){
					info = '<div class="text">'
						+ '<a onclick="TAIL.showDetails(this)">'+msg.msg+'</a>'
						+ msg.details.split("\n")[0]
						+ '</div>'
						var details = msg.details.split("\n");
						info += '<div class="details" style="display:none">'+details.join('<br>')+'</div>'
				}
				else info = '<div class="text">'+msg.msg+'</div>'
				messages.push('<div class="clearfix message ' + msg.level.toLowerCase() + '"><div class="time">'+msg.time+'</div>'
					+ '<div class="level">'+msg.level+'</div>'
					+ info + '</div>');
			}

			$("#msg").append(messages.join(""));
			if(autoscroll){

				while($("#msg").children().length>150){
					$("#msg").children().first().remove();
				}

				var height = $("#msg").height();
				var scrollHeight = $("#msg")[0].scrollHeight
				$("#msg")[0].scrollTop = scrollHeight-height
			}
		})
	}

	$(function(){

		$("#toolbar [data-action='play-pause']").click(function(){
			running ? pause() : play();
		})

		$("#msg").scroll(function(){
			if(!running)
				return;
			var height = $("#msg").height();
			var scrollHeight = $("#msg")[0].scrollHeight
			var scrollTop = $("#msg")[0].scrollTop

			if(scrollHeight != height+scrollTop){
				autoscroll=false
				$("#toolbar .hint").html("autoscroll stopped");
			}
			else {
				$("#toolbar .hint").html("");
				autoscroll=true
			}
		})


	})

	function play(){
		running=true;
		$("#toolbar .hint").html("");
		if(timer)
			clearTimeout(timer);
		timer = setInterval(load, 1000);
		scrollToBottom();
		$("#toolbar [data-action='play-pause'] span")
			.removeClass("glyphicon-play")
			.addClass("glyphicon-pause")
	}

	function pause(){
		running=false;
		if(timer){
			clearTimeout(timer);
			timer = null;
			$("#toolbar .hint").html("stopped");
		}
		$("#toolbar [data-action='play-pause'] span")
			.removeClass("glyphicon-pause")
			.addClass("glyphicon-play")
	}

	function scrollToBottom(){
		var height = $("#msg").height();
		var scrollHeight = $("#msg")[0].scrollHeight
		$("#msg")[0].scrollTop = scrollHeight-height
	};

	return {

		showDetails: function(el){
			var details = $(el).parents(".message").find(".details").html();
			$("#details .content").html(details);
			$.wga_modal.show("#details")
		},

		scrollToBottom: scrollToBottom,

		start: function(jsonurl){
			url = jsonurl
			play();
		}

	}

}()
