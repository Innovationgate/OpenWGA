/*
 *	jquery-plugin modal
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	var bodyMask;
	var currentModal;
	var onload;
	var onclose;
	var triggerEl;
		
	function maskBody(){
		if(!bodyMask){
			$("body").append('<div class="body-mask" id="modal-body-mask"></div>')
			bodyMask = $("#modal-body-mask").on("click.hide-modal", function(){hideModal()})
		}
		bodyMask.fadeIn({duration: 200})
	}
	
	function hideModal(callback){
		if(callback){
			onclose=callback;
		}
		var fn = $.wga_modal.effect == "slide" ? "slideUp" : "fadeOut"
		bodyMask && bodyMask.fadeOut({duration: 200});
		$(currentModal)[fn]({
			duration: 200,
			complete: function(){
				if(onclose)
					onclose();
				$(this).trigger("modal-closed");
				if(triggerEl)
					$(triggerEl).trigger("close", currentModal);
				triggerEl=null;
			}
		});
	}
	
	function showModal(id, callback){
		var el = $(id);
		if(!el.length){
			if(callback)
				callback();
			return alert("Modal with ID " + id + " not found");
		}
		if(!el.hasClass("modal-popup"))
			el.addClass("modal-popup");
		currentModal=id;
		maskBody();
		if(callback)
			onload=callback;
		var fn = $.wga_modal.effect == "slide" ? "slideDown" : "fadeIn"
		$(id)[fn]({
			duration: 200,
			complete: function(){
				if(onload)
					onload();
				$(this).trigger("modal-shown", triggerEl);
				if(triggerEl)
					$(triggerEl).trigger("load", id);
			}
		});
	}
	
	$.fn.wga_modal = function(config){
		var config = config||{};
		return this.each(function(){
			$(this).on("click", function(e){
				e.preventDefault();
				onload = config.onload;
				onclose = config.onclose;
				triggerEl=this;
				var target = config.target || this.hash
				if(config.width){
					$(target).css({
						width: config.width,
						marginLeft: -config.width/2 
					})
				}
				showModal(target);
			})
		})
	}
	

	$(document).on('click.wga_modal_close', ".modal-popup > .close, .modal-popup a.close-modal, [data-modal='hide']", function(e){
		e.preventDefault();
		hideModal();
	})

	// data interface:
	$(document).on('click.wga_modal_show', "[data-modal='show']", function(e){
		e.preventDefault();
		triggerEl=this;
		showModal(this.hash || $(this).data("target"));
	}) 

	// Globals
	$.wga_modal={
		hide: hideModal,
		show: showModal,
		effect: "fade"
	}

	return $.wga_modal

});
