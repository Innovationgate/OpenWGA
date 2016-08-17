/*
 *	jquery-plugin lightbox
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */
 
!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define([
    		'jquery',
    		"css!/plugin-wga-app-framework/css/jquery-lightbox"
    	], factory);
  	else factory(root.jQuery);
}(window, function($) {

	var bodyMask=null;
	var lightbox=null;
	var viewport={};
	var current_url, current_group;
	var images = {}

	function keyHandler(e){
		//console.log("key", e);
        var keyCode = e.keyCode
        if(
            keyCode == 88 || // x
            //keyCode == 67 || // c
            keyCode == 27
        ){
            hideImage();
        }
        else if(keyCode==37)	// cursor left 
        	prevImage();
        else if(keyCode==39)	// cursor right 
        	nextImage();
	}

	function maskBody(){
		$(document).on("keydown.image", keyHandler);
		if(!bodyMask){
			$("body").append('<div class="body-mask" id="lightbox-body-mask"></div>')
			bodyMask = $("#lightbox-body-mask").on("click.hide-images", hideImage)
			
			$("body").append('\
				<div id="lightbox">\
					<div class="controls">\
						<a class="lightbox-close"></a>\
						<a class="image-prev"></a>\
						<a class="image-next"></a>\
					</div>\
					<img>\
					<div class="image-loading">Loading ...</div>\
					<div class="image-info">\
						<div class="image-title"></div>\
						<div class="image-download"><a href="#">Download</a></div>\
						<div class="image-group"></div>\
					</div>\
				</div>\
			')

			lightbox = $("#lightbox");
			$("#lightbox a.lightbox-close").on("click", hideImage);
			$("#lightbox a.image-prev").on("click.next", prevImage)
			$("#lightbox a.image-next").on("click.next", nextImage)
			
			$("#lightbox img").on("load", function(){
				$("#lightbox .image-loading").hide();
				scaleImg(this);
				
				var img=$(this);
				lightbox.animate({
					left: (viewport.width-this.width)/2-10,
					width: this.width
				}, {
					duration: 200
				})
				.animate({
					top: (viewport.height-this.height)/2-40,
					height: this.height+60
				}, {
					duration: 200,
					complete: function(){
						img.animate({opacity: 1});						
						$("#lightbox .controls").show();
						$("#lightbox .image-info").fadeIn();
					}
				})
			})
			
		}
		bodyMask.show()
		viewport={
			width: bodyMask.width(),
			height: bodyMask.height()
		}
		lightbox.css({
			top: (viewport.height-200)/2,
			left: (viewport.width-400)/2,
			width: 400,
			height: 200
		})
	}
	function unmaskBody(){
		bodyMask && bodyMask.fadeOut();
		$(document).off("keydown.image");
	}

	function hideImage(){
		lightbox && lightbox.hide();
		unmaskBody();
	}

	function showImage(url, group, download, download_url){
		current_url=url;
		current_group=group;
		
		var index = getImageIndex(url, group)
		if(index==-1)
			return;
		if(index==0)
			$("#lightbox .image-prev").hide()
		else $("#lightbox .image-prev").show()
		if(index==images[group].length-1)
			$("#lightbox .image-next").hide()
		else $("#lightbox .image-next").show()
		
		var title = images[group][index].title;
		$("#lightbox .image-info").hide();
		$("#lightbox .controls").hide();
		$("#lightbox .image-title").html(title);
		$("#lightbox .image-group").html(getLabel("image_count").replace(/\{1\}/, index+1).replace(/\{2\}/,images[group].length)) 
		$("#lightbox .image-download a").attr("href", download_url + (download_url.indexOf("?")<0 ? "?":"&") + "forcedownload=true");
		
		if(download)
			$("#lightbox").addClass("download");
		else $("#lightbox").removeClass("download");
			
		$("#lightbox .image-loading").show();
		lightbox.show().find("img").css({
			opacity: 0,
			width: "auto",
			height: "auto"
		}).attr("src", "").attr("src", url);
	}

	function prevImage(){
		var index = getImageIndex(current_url, current_group)
		if(index==0)
			return;
		showImage(images[current_group][index-1].url, current_group, images[current_group][index-1].download, images[current_group][index-1].download_url);
	}
	function nextImage(){
		var index = getImageIndex(current_url, current_group)
		if(index==images[current_group].length-1)
			return;
		showImage(images[current_group][index+1].url, current_group, images[current_group][index+1].download, images[current_group][index+1].download_url);
	}

	function scaleImg(img){
		
		/*
		img.removeAttribute("width");
		img.removeAttribute("height");
		*/

		var $img = $(img);
		
		var wMax = viewport.width-70;
		var hMax = viewport.height-120;
	
		var ratio = $img.height()/$img.width();

		if($img.height()>hMax){
			var newW = Math.min(hMax / ratio, wMax);
			$img.width(newW);
			$img.height(newW * ratio);
		}		
		else if($img.width()>wMax){
			var newH = Math.min(ratio * wMax, hMax);
			$img.height(newH);
			$img.width(newH / ratio);
		}
	}

	function getImageIndex(url, group){
		if(images[group]){
			for(var i=0; i<images[group].length; i++){
				if(images[group][i].url==url)
					return i
			}
		}
		return -1;
	}

	// jquery plugin interface
	$.fn.lightbox = function(config){
		var config = config||{};
		return this.not(".WGA-Item-Value-Unencoded *").each(function(){
			var $this = $(this);
			var url = config.imageURL || $this.data("image-url") || this.href||this.src;
			url=url.split(";")[0];	// ignore ;jsessiond-s
			var title = config.title || $this.data("image-title")||this.title||this.alt||decodeURI(url.split("/").pop())
			var group = config.group||$this.data("image-group")||"default";
			var download = config.download || $this.data("image-download")
			var download_url = config.imageDownloadURL || $this.data("image-download-url")||url;
			
			if(config.remove){
				var index = getImageIndex(url, group)
				if(index>=0){
					images[group].splice(index, 1)
				}
				$this.off("click.lightbox")
			}
			else if(url){
				if(getImageIndex(url, group)==-1){				
					if(!images[group])
						images[group]=[]
					images[group].push({
						url: url,
						title: title,
						download: download,
						download_url: download_url
					});
				}
				$this.on("click.lightbox", function(e){
					e.preventDefault();
					e.stopPropagation();
					if(!images[group] || !images[group].length)
						return;
					maskBody();
					showImage(url, group, download, download_url);
				})
				.css("cursor", "pointer");
			}
		})
	}

	// Globals
	$.lightbox={
		lang: "en",
		labels: {
			"de": {
				"image_count": "Bild {1} von {2}"
			},
			"en": {
				"image_count": "Image {1} of {2}"
			}
		},
		deleteImage: function(url, group){
			var group = group||"default"
			var index = getImageIndex(url, group)
			if(index>=0){
				images[group].splice(index, 1)
			}
		},
		deleteGroup: function(group){
			delete images[group||"default"];
		},
		destroy: function(){
			hideImage();
			images={}			
		}
	}
	function getLabel(label){
		var labels = $.lightbox.labels[$.lightbox.lang] || $.lightbox.labels["en"]
		return labels[label]
	}

	// special hijax event of BWK2:
	$(window).on("page-unload", function(){
		$.lightbox.destroy();	
	})

	return $.lightbox; 
	
});
