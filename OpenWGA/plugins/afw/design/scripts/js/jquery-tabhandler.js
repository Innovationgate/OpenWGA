/*
 * 	jQuery Textarea TAB Handler
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

!function(root, factory) {
  	if(typeof define === 'function' && define.amd)
    	define(['jquery'], factory);
  	else factory(root.jQuery);
}(window, function($){

	function getLineAtCursor(el){
		var pos = el.selectionStart;
		var lines = el.value.split("\n");
		for(var start=0, i=0; i<lines.length; i++){
			var line = lines[i];
			var length = line.length+1;		// count one \n here
			if(start<=pos && pos<start+length)
				return line
			start += length
		}
	}
	
	function insertAtCursor(el, text){
		var pos = el.selectionStart;
		var endPos = el.selectionEnd;
		el.value = el.value.substr(0, pos) + text + el.value.substr(endPos);
        el.selectionEnd = pos + text.length;
	}
	
	function handleTab(el, shiftKey){
		var pos = el.selectionStart;
	    var endPos = el.selectionEnd;
     	var selectionText= el.value.substring(pos, endPos);
     	
     	if(pos==endPos && !shiftKey){
	        el.value = el.value.substr(0, pos) + "\t" + el.value.substr(endPos);
	        el.selectionEnd = pos + 1;
     	}     	
     	else{
     		// multi line Tab
			var lines = el.value.split("\n");
			var firstLine, lastLine,
				firstPos, lastPos;
			
			for(var start=0, i=0; i<lines.length; i++){
				var line = lines[i];
				var length = line.length+1;		// count one \n here
				//console.log(i, line, length, pos);
				if(start<=pos && pos<start+length){
					firstLine = i;
					firstPos = pos-start;
				}
				if(start<=endPos && endPos<start+length){
					lastLine = i;
					lastPos = endPos-start;
					break;
				}
				start += length
			}
			//console.log(firstLine, lastLine, pos, endPos, firstPos);
			var posIncrement=0;
			for(var i=firstLine; i<=lastLine; i++){
				if(shiftKey){
					if(lines[i].substr(0,1)=="\t"){
						lines[i] = lines[i].substr(1);
						if(lastPos > 0)
							endPos--;
						posIncrement = (firstPos>0 ? -1 : 1)
					}
				}
				else{
					lines[i] = "\t" + lines[i];
					endPos++;
					posIncrement = 1
				}
			}
			pos += posIncrement;
			el.value = lines.join("\n");
			el.selectionStart = pos;
			el.selectionEnd = endPos;
     	}
        	
	}

	function handleEnter(el){
		var tabs="";
		var line = getLineAtCursor(el)
		for(var i=0; i<line.length; i++){
			if(line.charAt(i)=="\t")
				tabs += "\t";
			else break;
		}
		insertAtCursor(el, "\n"+tabs)
	}
	
	$.fn.tabhandler = function(){
		return this.each(function(){
			$(this).keydown(function(ev){
				if(ev.keyCode==9){
					ev.preventDefault();
					handleTab(this, ev.shiftKey)
				}
				else if(ev.keyCode==13){
					ev.preventDefault();
					handleEnter(this)
				}
			});
		})
	}

})