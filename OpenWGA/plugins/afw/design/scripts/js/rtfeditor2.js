/*******************************************************************************
 *Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 *
 *This file is part of the OpenWGA server platform.
 *
 *OpenWGA is free software: you can redistribute it and/or modify
 *it under the terms of the GNU General Public License as published by
 *the Free Software Foundation, either version 3 of the License, or
 *(at your option) any later version.
 *
 *In addition, a special exception is granted by the copyright holders
 *of OpenWGA called "OpenWGA plugin exception". You should have received
 *a copy of this exception along with OpenWGA in file COPYING.
 *If not, see <http://www.openwga.com/gpl-plugin-exception>.
 *
 *OpenWGA is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with OpenWGA in file COPYING.
 *If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

if(typeof(AFW)=="undefined")
	AFW={};

AFW.RTF={};

AFW.RTF.getURLInfo=function(tag){
	var linktype=tag.getAttribute("linktype");
	var wgakey=tag.getAttribute("wgakey");

	var parts = WGA.util.decodeURI(tag.href||tag.src||"").split("://");	
	var protocol, path;
	if(parts.length>1){
	 	protocol = parts[0];
	 	path = parts[1]
	}
	else{
	 	protocol = "";
	 	path = parts[0]
	}

	path = path.split("?")[0]	// remove URL params
	path = path.split("/")			

	var domain=path.shift().split(":")[0];	// remove port

	if(linktype==null || wgakey==null){
		// CM 1.4 produces attribute wga:urltype with value type|key
		var urlinfo = tag.getAttribute("data-wga-urlinfo") || tag.getAttribute("wga:urlinfo");
		if(urlinfo){
			var a = urlinfo.split("|");
			linktype=a[0];
			wgakey=a[1];
		}
		else{
			// CM 1.3 style:
			// link has class="wga-urltype-<linktype>"
			linktype=WGA.util.getURLTypeFromClassName(tag.className)||"exturl";
			
			if(linktype=="file" || linktype=="extfile"){
				var filename=path[path.length-1]
				var container=path[path.length-2]
				var dbkey=path[path.length-4]
				wgakey=container+"/"+filename;
				if(dbkey)
					wgakey += ("/"+dbkey); 
				tag.setAttribute("wga:urlinfo", "extfile|"+wgakey);
			}			
			else if (linktype=="exturl")
				wgakey=tag.href || tag.src;
			else wgakey=path[path.length-1];		// last element in path
		}
	}

	/*
	console.log(tag, {
		type: linktype,
		key: wgakey,
		
		protocol: protocol,
		domain: domain,
		path: path
	}) 
	*/

	return {
		type: linktype,
		key: wgakey,
		
		protocol: protocol,
		domain: domain,
		path: path
	}
}

AFW.RTF.setURLInfo = function(tag, info){
	tag.setAttribute("data-wga-urlinfo", info.type+"|"+info.key);
	tag.setAttribute("wga:urlinfo", info.type+"|"+info.key);
}	

/**
 * RTF Editor
 * @id: the id of the element to edit.
 * @config: optinal config options
 */
AFW.RTF.editor=function(id, config){

	//var events = ["keydown", "mouseup"];
	var events = ["paste", "keydown", "keypress"];	// added "keypress" for paste handler in WebKit	
	var doc_events = ["focus", "blur", "mouseup"];
	function registerEvents(){
		for (var ev in events){
			if(ie)
				editor.editelement.attachEvent("on"+events[ev], __processEventCaller);
			else {
				var el = editor.iframe ? editor.doc : editor.editelement;
				el.addEventListener(events[ev], __processEventCaller, true);
			}
		}		
		for (var ev in doc_events){
			if(ie)
				editor.doc.attachEvent("on"+doc_events[ev], __processEventCaller);
			else editor.doc.addEventListener(doc_events[ev], __processEventCaller, true);
		}		
	}
	function unregisterEvents(){
		for (var ev in events){
			if(ie)
				editor.editelement.detachEvent("on"+events[ev], __processEventCaller);				 
			else{
				var el = editor.iframe ? editor.doc : editor.editelement;
				el.removeEventListener(events[ev], __processEventCaller, true);
			}				 
		}		
		for (var ev in doc_events){
			if(ie)
				editor.doc.detachEvent("on"+doc_events[ev], __processEventCaller);				 
			else editor.doc.removeEventListener(doc_events[ev], __processEventCaller, true);				 
		}		
	}

	function __processEventCaller(event){
		return editor._processEvent(event);
	}

	//+++++++++++++++++++++++++++
	// Constructor
	//+++++++++++++++++++++++++++

	var ie=WGA.isIE;
	var editor=this;
	var resizeIframeTimer;
	
	this.ensureParagraphs=false;
	
	var document=config.document||window.document;
	var contentinfo=config.contentinfo||WGA.contentinfo;
	
	this.dbkey = contentinfo.dbkey;
	this.contentkey = contentinfo.contentkey;
	
	this.id=id;
	this.toolbar=null;	
	this.autofocus = (config.autofocus==undefined)?true:config.autofocus;		
	this.viewmode="wysiwyg";

	this.pasteUnformatted = (config.pasteUnformatted == undefined ? false : config.pasteUnformatted); 
	
	this.orgElement=document.getElementById(id);
	this.unencodedElement=document.getElementById(id + "_unencoded");

	/*
	create new DOM elements. The DOM structure looks like this when finished:
	parent element
	|-span element 					this is a <div> hidden in viewmode "html" and "preview".
	| |-editelement 				<div contenteditable=true> in case of IE and Safari, <iframe> in case of Mozilla
	|-sourcecode element 			(textarea) - visiable only in viewmode "html"
	|-original element 				identified by the given ID - hidden in modes "wysiwyg" and "html", visable in mode "preview"
	*/
	
	this.spanElement=document.createElement("div");
	this.spanElement.style.display="none";
	this.orgElement.parentNode.insertBefore(this.spanElement, this.orgElement);

	// add iframe to handle paste event via CMD-V
	var pasteIframe = createPasteIframe();
	this.orgElement.parentNode.insertBefore(pasteIframe.element, this.orgElement);
		
	this.sourcecodeElement=this.orgElement.parentNode.insertBefore(document.createElement("textarea"), this.orgElement);
	this.sourcecodeElement.style.display="none";
	this.sourcecodeElement.style.width="100%";
	this.sourcecodeElement.rows="20";
	this.sourcecodeElement.spellcheck=false;
	this.sourcecodeElement.autocomplete=false;
	
	// create editor area

	var edit_style_helper = ".edit-helper div, .edit-helper p, .edit-helper ol, .edit-helper ul{\
			border-right: solid gray 1px;\
			border-left: none;\
			margin-right: 1px;\
			-moz-border-radius: 4px;\
			-webkit-border-radius: 4px;\
		}\
		.edit-helper div:before, .edit-helper h1:before, .edit-helper h2:before, .edit-helper ol:before, .edit-helper ul:before, .edit-helper p:before{\
			background: silver;\
			border: solid gray 1px;\
			color: white;\
			font-size:10px;\
			line-height: 10px;\
			font-weight: normal;\
			float:right;\
			margin-right: 1px;\
			padding: 0 4px;\
			-moz-border-radius: 4px;\
			-webkit-border-radius: 4px;\
			font-style: italic;\
		}\
		.edit-helper div:before{content:'DIV';}\
		.edit-helper p:before{content:'P';}\
		.edit-helper h1:before{content:'H1';}\
		.edit-helper h2:before{content:'H2';}\
		.edit-helper ol:before{content:'OL';}\
		.edit-helper ul:before{content:'UL';}\
	";
	
	if(WGA.fireFoxVersion()>=22 || WGA.isIE || WGA.isTrident || WGA.isWebKit){
	
		this.editelement=document.createElement("div");
		this.editelement.className = "content-edit";
		this.editelement.style.minHeight=20+"px"
		if(config.editHelper)
			this.editelement.className += " edit-helper";
		
		this.editelement.style.padding="3px";
		if(WGA.isWebKit){
			this.editelement.style.border="dotted red 1px";
			this.editelement.style.margin="-2px";		// substracts 1px border and 1px padding.
			this.editelement.style.WebkitBorderRadius="3px";
		}
		else {
			this.editelement.style.outline="none";
			this.editelement.style.border="1px solid #66afe9";
			this.editelement.style.boxShadow="0 1px 1px rgba(0, 0, 0, 0.075) inset, 0 0 8px rgba(102, 175, 233, 0.6)";
			this.editelement.style.borderRadius="3px";
			this.editelement.style.margin="-4px";		// substracts 3px border and 1px padding.
		}

		// create temporary <style> element in order to show td-s in edit mode.
		var head = document.getElementsByTagName("head")[0];
		var style_el = document.createElement("style");
		head.appendChild(style_el);
		
		var css = ".content-edit td{border: dotted silver 1px;}" + edit_style_helper;
		if(style_el.styleSheet){// IE
			style_el.styleSheet.cssText = css;
		}
		else {// w3c
			var style_content = document.createTextNode(css)
			style_el.appendChild(style_content);				
		}
			
		var content = this.unencodedElement.innerHTML;
		this.editelement.innerHTML=(content.replace(/\s/g, "")=="" ? "<br>" : content);		//content;
		this.spanElement.appendChild(this.editelement);
		this.editelement.contentEditable = true;
		
		this.orgElement.style.display="none";
		this.spanElement.style.display="block";
		
		if(this.autofocus){
			try{
				setFocus();
			}
			catch(e){
				console.log("RTF Editor: unable to set autofocus", e);
			}
		}
		this.doc=document;
		
		document.execCommand('styleWithCSS', false, true);

		//document.execCommand("enableObjectResizing", false, "false");
		document.execCommand("enableInlineTableEditing", false, "false");

		registerEvents();
		if(config.onready)
			config.onready(this);
	}
	else{	// Code for Mozilla:
		this.iframe=document.createElement("iframe");
		this.iframe.width="100%";
		this.iframe.height=this.orgElement.offsetHeight;
		this.iframe.style.border="3px solid #7eadd9";
		this.iframe.style.padding="1px";
		this.iframe.style.margin="-4px";
		this.iframe.style.MozBorderRadius="4px";
		this.iframe.style.borderRadius="4px";
		this.iframe.style.boxShadow="0 0 5px gray";
		
		this.iframe.scrolling="no"
		this.spanElement.appendChild(this.iframe);
		this.editelement=this.iframe;
		
		// set id for robot tests
		this.editelement.id = "rtf_" + this.id;

		// automaticaly resize iframe height
		function resizeIframe(){
			try{
				var h=editor.doc.documentElement.scrollHeight;
				
				if (editor.iframe.height!=h){
					editor.iframe.height=h;
					editor.iframe.contentWindow.scrollTo(0, 0);
				}
				if (editor.doc.designMode != "on")
					editor.doc.designMode = "on";
				if(editor.iframe.contentWindow)
					resizeIframeTimer = window.setTimeout(resizeIframe, 250);
			}
			catch(e){
				// try-catch in case of error
			}
		}
		
		function initIframe(){
			if(!editor.iframe)
				alert("iframe not found");
			if(!editor.iframe.contentWindow)
				alert("iframe has not contentWindow");
		
			var doc = editor.iframe.contentWindow.document;
			if(doc==null)
				alert("iframe has no document");
			try{
				doc.designMode = "on";
			}catch(e){
				//alert(e + "\ndesignmode not avaliable");
			}
				
			doc.open();
			
			// calulate default text styles:
			var textcolor = computedStyle(editor.orgElement, "color");
			var fontsize = computedStyle(editor.orgElement, "fontSize");
			var fontfamily = computedStyle(editor.orgElement, "fontFamily");
			var defaultstyle="body{"
			if(textcolor)
				defaultstyle += "color:"+textcolor+" !important;"
			if(fontfamily)
				defaultstyle += "font-family:"+fontfamily+" !important;";
			if(fontsize)
				defaultstyle += "font-size:"+fontsize+" !important;";
			defaultstyle += "}";
			/*
			if(fontsize)
				defaultstyle += "body, body div, body span, body td, body p{font-size:"+fontsize+";}";
			*/
			var links = editor.orgElement.getElementsByTagName("a");
			if(links && links.length){
				var link_font_size = computedStyle(links[0], "fontSize");
				var link_color = computedStyle(links[0], "color");				
				defaultstyle+="a{"
				if(link_font_size)
					defaultstyle += "font-size:"+link_font_size+" !important;"					 
				if(link_color)
					defaultstyle += "color:"+link_color+" !important;"					 
				defaultstyle+="}"
			}

			defaultstyle += edit_style_helper;
			var html = "<html>\n";
			html += "<head>\n";
			html += "<base href='" + document.location + "'>";
			html += "<style>" + defaultstyle + ".content-edit td{border:dotted 1px silver}</style>\n";
			if (config.css){
				html += "<link type='text/css' rel='stylesheet' href='" + config.css + "'>\n" 
			}
			if (config.style){
				html += "<style>" + config.style + "</style>\n" 
			}
			html += "</head>\n";
			html += "<body class=\"content-edit rtf"
			if(config.editHelper)
				html += " edit-helper";
			html += "\" style=\"margin:0px;background:transparent;\">";
			html += "</body>\n";
			html += "</html>";
			doc.write(html);
			doc.close();

			editor.doc=doc;
			
			editor.iframe.contentWindow.addEventListener("load", function(){
				var body = doc.getElementsByTagName("body")[0];
				var content="";
				if (editor.unencodedElement.tagName=="TEXTAREA")
					content=editor.unencodedElement.value;
				else content=editor.unencodedElement.innerHTML;
				body.innerHTML = (content.replace(/\s/g, "")=="" ? "<br>" : content);
				
				editor.orgElement.style.display="none";
				editor.spanElement.style.display="block";
				
				resizeIframeTimer = window.setTimeout(resizeIframe, 250);

				if(editor.autofocus)
					editor.focus();
	
				registerEvents();
				doc.execCommand('styleWithCSS', false, false);
	
				if(config.onready)
					config.onready(editor);
			}, false)
		}
		setTimeout(initIframe, 10);
	}


	//-------- end Constructor----------------
	
	this.cleanHTML = function(){
		var html = AFW.RTF.getCleanInnerHTML(editor.editelement);
		editor.editelement.innerHTML = html;
		editor.doc.defaultView.focus();
	}


	this.showEditHelper = function(show){
		var el = this.iframe ? this.doc.body : this.editelement;
		if(show){
			if(el.className.indexOf("edit-helper")<0)
				el.className = el.className + " edit-helper";
		}
		else el.className = el.className.replace(/ edit-helper/, "");
	}
	this.toggleEditHelper = function(){
		var el = this.iframe ? this.doc.body : this.editelement;
		if(el.className.indexOf("edit-helper")>=0)
			el.className = el.className.replace(/ edit-helper/, "");
		else el.className = el.className + " edit-helper";
	}
	this.hasEditHelper = function(){
		var el = this.iframe ? this.doc.body : this.editelement;
		return el.className.indexOf("edit-helper")>=0
	}

	this.stopResizeIframeTimer=function(){
		if(resizeIframeTimer){
			window.clearTimeout(resizeIframeTimer);
		}
	}
	this.restartResizeIframeTimer=function(){
		if(this.iframe)
			resizeIframeTimer = window.setTimeout(resizeIframe, 250);
	}

	/*
	 * Helper function to get computed style
	 */
	function computedStyle(_elem, _style)
	{
		var computedStyle={};
		try{
			if (typeof _elem.currentStyle != 'undefined')
				computedStyle = _elem.currentStyle;
			else computedStyle = document.defaultView.getComputedStyle(_elem, null);
		}
		catch(e){}
		return computedStyle[_style];
	}
	
	/*
	 * check for parent that is a block element
	 */
	this.getParagraph=function(el){
		var para_tags = "p,h1,h2,h3,h4,h5,h6,pre";
		if(!this.isEditorSelected())
			return null;
		try{
			var ptag = el || this.getParentNodeFromSelection();
			while(ptag && ptag.tagName!="BODY" && ptag.tagName!="TD" && ptag.tagName!="BLOCKQUOTE" && ptag!=editor.editelement){
				if(ptag.nodeType==1 && para_tags.indexOf(ptag.tagName.toLowerCase())>=0)
					return ptag;
				ptag=ptag.parentNode;			
			}
		} catch(e){};
		return null;
	}

	this.getWidth=function(){
		return this.editelement.offsetWidth;
	}
	this.getHeight=function(){
		return this.editelement.offsetHeight;
	}

	/**
	 * convert given html code and create scriptlets for links and images
	 * @param String: htmltext
	 */
	this.makeScriptlets=function(htmltext){

		var document=config.document||window.document;
		var contentinfo=config.contentinfo||WGA.contentinfo;	
		var contentkey = contentinfo?contentinfo.contentkey:myContentKey;	// old style BI3 way
		var dbkey = contentinfo?contentinfo.dbkey:myDbKey;	// old style BI3 way

		/* internal Helper function used to convert old imgage urls into old scriptlet code */
		function convertWGAKeysToScriptlets(htmltext){
			//console.log("convertWGAKeysToScriptlets", htmltext);
			var server_host = self.location.hostname;
			var server_port = self.location.port;
			var server_protocol = self.location.protocol;
			var dbkey = editor.dbkey;
			
			var result=htmltext;
			
			// Replace contentKey
			
			// var keyRegExp = new RegExp(editor.contentkey.replace( /\./g, "\\.") ,'gi');
			// B00004486: problems with notes-migrated RTF-s
			// changed the regExp to ignore version  
			var k = contentkey.split(".");
			k.pop();				// remove last part (version)
			k.push("\\d+");			// add wildcard (any number) for version
			
			var keyRegExp = new RegExp(k.join("\\.")+"/", 'gi');	
			result = result.replace(keyRegExp , "{%$key%}/");
			
			// Replace absolute URLs by relative URLs		
			var tmpPort = server_port;
			if( tmpPort && tmpPort != null && tmpPort!=''){
				tmpPort = ":" + tmpPort;
			}

			// replace absolute URL-s containing dbkey:
			keyRegExp = new RegExp(server_protocol + "\/\/" +server_host + tmpPort + WGA.contextpath + "/" + dbkey,'gi' );
			result = result.replace(keyRegExp , "../../../{%$db:dbkey%}"); 
			
			// replace nearly absolute URL-s (without protocoll:server:port) containing dbkey:
			keyRegExp = new RegExp(WGA.contextpath + "/" + dbkey, 'gi');
			result = result.replace(keyRegExp , "../../../{%$db:dbkey%}"); 
			
			// Replace dbKeys in relative URLs entered by user ( MUST be executed AFTER absolute URLs. )
			keyRegExp = new RegExp("/" + dbkey.replace( /\./g, "\\.") + "/" ,'gi');	
			result = result.replace(keyRegExp , "/{%$db:dbkey%}/");
			
			//console.log("result-end", result);
			return result; 
		}

		// use browsers dom as HTML parser:
		var el = document.createElement("div");
		el.innerHTML = htmltext;
		
		// convert link urls to scriptlets
		var links = el.getElementsByTagName("a");
		for(var i=0; i<links.length; i++){
			var link = links.item(i);
			
			// calculate linktype and wgakey:
			var info = AFW.RTF.getURLInfo(link);
			var linktype=info.type;
			var wgakey=info.key;

			switch(linktype){
				/*
				 * Don't directly write URL to link.href because FF3 will encode this URL in this case
				 * So we write the URL to an Attribute "wga:href" that will later be removed by a regexp
				 */
				case "int":
					link.setAttribute("wga:href", "{%!contenturl:"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "intname":
					link.setAttribute("wga:href", "{%!namedcontenturl:"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "intfile":
					link.setAttribute("wga:href", "{%!fileurl:"+wgakey+"%}");
					link.removeAttribute("href");
					break;
				case "file":
				case "extfile":
					var key = wgakey.split("/");
					var container = key[0];
					var filename = key[1];
					var dbkey = key[2];
					var scriptlet = "{%"
					if(dbkey)
						scriptlet += "(db:"+dbkey+")"
					scriptlet += "!fileurl:"+container+","+filename+"%}"
					link.setAttribute("wga:href", scriptlet);
					link.removeAttribute("href");
					break;
			}

		}

		// convert IMG urls to scriptlets
		var imgs = el.getElementsByTagName("img");
		for(var i=0; i<imgs.length; i++){
			var img = imgs.item(i);
			// calculate linktype and wgakey:
			var info = AFW.RTF.getURLInfo(img);
			var urltype=info.type;
			var wgakey=info.key;
			switch(urltype){
				case "file":
				case "extfile":
					var key = wgakey.split("/");
					var container = key[0];
					var filename = key[1];
					var dbkey = key[2];

					if(img.style){
						if(img.style.width){
							var i = img.style.width.indexOf("px");
							if(i)
								filename += "?width~"+img.style.width.substr(0, i);
						}
						else if(img.style.height){
							var i = img.style.height.indexOf("px");
							filename += "?height~"+img.style.height.substr(0, i);
						}
					}
					
					var scriptlet = "{%"
					if(dbkey)
						scriptlet += "(db:"+dbkey+")"
					scriptlet += "!imgurl:"+container+","+filename+"%}"
					img.setAttribute("wga:src", scriptlet);
					
					scriptlet = "{%"
					if(dbkey)
						scriptlet += "(db:"+dbkey+")"
					scriptlet += "!srcset:"+container+","+filename+"%}"
					img.setAttribute("wga:srcset", scriptlet);
					
					img.removeAttribute("src");
					img.removeAttribute("srcset");
					break;
				case "intfile":
					if(img.style){
						if(img.style.width){
							var i = img.style.width.indexOf("px");
							if(i)
								wgakey += "?width~"+img.style.width.substr(0, i);
						}
						else if(img.style.height){
							var i = img.style.height.indexOf("px");
							wgakey += "?height~"+img.style.height.substr(0, i);
						}
					}
					
					img.setAttribute("wga:src", "{%!imgurl:"+wgakey+"%}");
					img.setAttribute("wga:srcset", '{%!srcset:' + wgakey + '%}');
					img.removeAttribute("src");
					img.removeAttribute("srcset");
					break;
				case "exturl":
					/*
					 * B00004D6A
					 * WGA 4.1.1: convert only old style images instead of global convertWGAKeysToScriptlets()
					 * old style img-tags from wga 4.0:
					 */
					
					var converted_src = convertWGAKeysToScriptlets(img.src);
					/*
					 * check if it has been converted to an internal image-url.
					 * treat it as "intfile" in this case and convert to new style  
					 */
					if(converted_src.indexOf("{%$key%}")>=0){
						// we interpret this as internal image:
						var wgakey = converted_src.split("/").pop();	// last element in path
						img.setAttribute("wga:src", "{%!imgurl:"+wgakey+"%}");
						img.setAttribute("wga:urlinfo", "intfile|"+wgakey);

						var c = img.className;
						c = c.replace(/wga-urltype-\w+ */, "");
						img.className = c + (c?" ":"") + "wga-urltype-intfile"

					}
					else img.setAttribute("wga:src", converted_src);
					img.removeAttribute("src");
					break;
			}
		}

		var htmltext = el.innerHTML;
		htmltext = htmltext.replace(/wga:href="([^"]*)"/g, 'href="$1"') 
		htmltext = htmltext.replace(/wga:src="([^"]*)"/g, 'src="$1"')
		htmltext = htmltext.replace(/wga:srcset="([^"]*)"/g, '$1') 

		// CM 1.4: embed new attribute wga:urlinfo in scriptlet 
		htmltext = htmltext.replace(/wga:urlinfo="([^"]*)"/g, '{%!rtfsystem:wga:urlinfo="$1"%}');

		return htmltext;
	}

	
	this.getRTFHTML=function(){
		return this.makeScriptlets(this.getHTML());
	}
	this.setRTFHTML=function(html_unencoded, html_encoded){
		this.orgElement.innerHTML = html_encoded		
		this.unencodedElement.innerHTML = html_unencoded
		this.editelement.innerHTML = html_unencoded
	}
	
	
	this.changeViewMode=function(mode){
		switch(mode){
			case "wysiwyg":
				if (this.viewmode=="html")
					editor.setHTML(editor.sourcecodeElement.value);
				editor.sourcecodeElement.style.display="none";
				editor.orgElement.style.display="none";
				editor.spanElement.style.display="block";
				if (this.toolbar && this.toolbar.disableAllButtons)
					this.toolbar.disableAllButtons(false);
				//setFocus();
			break;
			case "html":
				editor.sourcecodeElement.value=editor.getHTML();
				editor.orgElement.style.display="none";
				editor.spanElement.style.display="none";
				editor.sourcecodeElement.style.display="inline";	// MUST be "inline". if "block" is used, Mozilla positions textarea over toolbar!
				if (this.toolbar && this.toolbar.disableAllButtons)
					this.toolbar.disableAllButtons(true);
				editor.sourcecodeElement.focus();
			break;
			case "preview":
				if (this.viewmode=="html")
					editor.setHTML(editor.sourcecodeElement.value);
				editor.sourcecodeElement.style.display="none";
				editor.orgElement.style.display="block";
				editor.spanElement.style.display="none";
				editor.orgElement.innerHTML=String(editor.getHTML());
				if (this.toolbar && this.toolbar.disableAllButtons)
					this.toolbar.disableAllButtons(true);
			break;			
		}
		this.viewmode=mode;
		if(this.onViewModeChange)
			this.onViewModeChange(mode)
	}	

	function handleEnter(ev){
		var range = editor.getRange();
		
		if(ev.shiftKey){
			// insert <br>:
			//console.log("BR -> jetzt");
			var br=editor.doc.createElement("br");
			var tn=editor.doc.createTextNode("");
			range.insertNode(br)
			range.setStartAfter(br);
			range.insertNode(tn)
			range.setStartAfter(tn);
			range.collapse(true);
			editor.setRange(range);
			_stopEvent(ev);
			return;
		}
		 
		var li = editor.getNearestTagFromSelection("li")
		var para = editor.getParagraph();
		if(li && !para){
			// spezial handling for lists:			
			if(li.innerHTML.replace(/<br>/g, "")==""){
				if(li.parentNode.tagName=="UL")
					editor.execCmd("InsertUnorderedList");
				else editor.execCmd("InsertOrderedList");
				editor.execCmd("FormatBlock", "p");
				_stopEvent(ev);
			}
			return;
		}

		// try to calculate if we are on "end of line":
		if(!para){
			editor.execCmd("FormatBlock", "p");
			para = editor.getParagraph();
			range = editor.getRange();
		}
		var sc = range.startContainer;
		var so = range.startOffset;
		var isEndOfLine = true;
		var selectedNode = (sc.nodeType==3 /* Text Node */ ? sc: sc.childNodes[so]||sc.lastChild||sc)
		if(selectedNode.nodeType=="1" || (selectedNode.nodeType=="3" && so<sc.length))
			isEndOfLine = false;
		else {
			for(var node=selectedNode; node!=para; node=node.parentNode){
				if(node.nextSibling){					
					isEndOfLine = false;
					if(node.nextSibling.nodeName=="BR"){
						range.setStartBefore(node.nextSibling)
					}
					break;
				}
			}
		}

		if(isEndOfLine){
			//console.log("EOL -> jetzt", para);
			range.deleteContents();
			var el=editor.doc.createElement("p");
			var br=editor.doc.createElement("br");
			var tn=editor.doc.createTextNode("");
			el.appendChild(tn);
			el.appendChild(br);
			
			if(para==selectedNode)
				range.setStartBefore(para);
			else range.setStartAfter(para);
			
			range.insertNode(el)
			range.setStartAfter(tn);
			range.setEndAfter(tn);
			range.collapse(true);
			editor.setRange(range);
			_stopEvent(ev);
		}

	}

	var lastEventType;
	this._processEvent=function (ev){

		this.selection.save();
	
		if(ev.type=="paste" && ev.clipboardData){
			var dt = ev.clipboardData
			var has_html = dt.types.indexOf ? (dt.types.indexOf("text/html")>=0) 
					: dt.types.contains ? (dt.types.contains("text/html"))
					: false;
			var has_rtf = dt.types.indexOf ? (dt.types.indexOf("text/rtf")>=0) 
					: dt.types.contains ? (dt.types.contains("text/rtf"))
					: false;
			
			var html = dt.getData("text/html")
			var plain = dt.getData("text/plain")

			ev.preventDefault();

			if(has_rtf && !has_html){
				if(!confirm("Your Clipboard seems to contain formatted text. You should cancel and use CMD-V to insert it as cleaned HTML.\n\nPaste plain text instead?")){
					ev.preventDefault();
					return;
				}
			}
			
			if(html){
				var div = document.createElement("div");
				div.innerHTML = html
				var html = AFW.RTF.getCleanInnerHTML(div, editor.toolbar==null)
				if(html){
					editor.insertHTML(html);
				}
			}
			else if (plain) {
				var encoded_plain = plain.replace(/&/g, '&amp;')
					.replace(/</g, '&lt;')
					.replace(/>/g, '&gt;')
					.replace(/\n/g, "<br>")
				editor.insertHTML(encoded_plain);
			}
			else return;	// no other formats supported.
			
			var range = editor.getRange();
			editor.doc.defaultView.focus();
			editor.setRange(range);
		}		
	
		if(ev.type=="paste" && this.pasteUnformatted && window.clipboardData){
			var data=window.clipboardData.getData("text");
			if(data){
				window.clipboardData.setData("Text", data);
			}
			else ev.returnValue = false;
		}		
	
		//console.log("event " + ev.type + "/" + ev.keyCode);		
		lastEventType=ev.type;
		
		// webkit does not select images: do it yourself:
		if(ev.type=="mouseup" && ev.target && ev.target.tagName=="IMG"){			
			var imgel = ev.target;
			if(imgel){			
				var sel = _getSelection();
				var range = _createRange(sel)

				range.setStartBefore(imgel);
				range.setEndAfter(imgel);
				
				sel.removeAllRanges()
				sel.addRange(range);
			}
		}

		// handle paste (CMD-left on FireFox)
		// prevent FireFox Default (browser back)
		if(WGA.isGecko && ev.type=="keydown" && ev.keyCode==37 && ev.metaKey &&!ev.shiftKey){
			ev.preventDefault();
		}

		// handle paste (CMD-V)
		// don't wait for "keydown" becaus this doesn't work in Safari.
		if(ev.type=="keypress" && (ev.charCode==118||ev.keyCode==118/*IE*/) && (ev.metaKey||ev.ctrlKey)){
			//console.log("CMD-V", ev);
			pasteIframe.doPaste();
		}

		// handle tab:
		if(ev.type=="keydown" && ev.keyCode==9){
			var para = this.getParagraph();
			console.log(para, para && para.tagName)
			if(para && para.tagName!="PRE"){
				if(ev.shiftKey)
					this.execCmd("Outdent");
				else this.execCmd("Indent");
			}
			else editor.insertHTML("\t")
			_stopEvent(ev);
		}

		// handle ENTER
		if(!WGA.isIE && ev.type=="keydown" && ev.keyCode==13){
			handleEnter(ev);
		}

		if(this.ensureParagraphs && !this.getParagraph()){
			if(editor.getHTML()=="<br/>" || editor.getHTML()=="<br>")
				editor.setHTML("");
			this.execCmd("FormatBlock", "p");
		}
		
		// update the toolbar state after some time
		if (this.toolbar){
			switch(ev.type){
				case "focus":
				case "blur":
					if(this.toolbar.editorGotFocus && this.isEditorSelected())
						this.toolbar.editorGotFocus();
					else if(this.toolbar.editorLostFocus && !this.isEditorSelected())
						this.toolbar.editorLostFocus();
					break;
				case "keydown":
					if(ev.altKey && this.toolbar.doKeyFunction && this.toolbar.doKeyFunction(ev))
						_stopEvent(ev);
					break;
			}
			if (this._timerToolbar) {
				clearTimeout(this._timerToolbar);
			}
			this._timerToolbar = setTimeout(function() {
				/*if(!editor.getParagraph() && !editor.getNearestTagFromSelection("div"))
					editor.execCmd("FormatBlock", "p");*/
				editor.toolbar.update(lastEventType);
				editor._timerToolbar = null;
			}, 250);
		}

	}
	
	function _stopEvent(ev) {
		if (ie){
			ev.cancelBubble = true;
			ev.returnValue = false;
		}
		else {
			ev.preventDefault();
			ev.stopPropagation();
		}
	}
	
	//-----------------------
	
	this.focus=function(){
		setFocus();
		if(this.selection)
			this.selection.restore();
	}	
	function setFocus(){
		editor.editelement.focus();
	}

	// returns the current selection object
	function _getSelection() {
		try{
			if (ie) {
				return editor.doc.selection;
			} else {
				return editor.doc.defaultView.getSelection();
			}	
		}
		catch(e){
			//console.trace();
			return null;
		}	
	}
	this.getSelection = _getSelection();
	
	// returns a range for the current selection
	function _createRange(sel) {
		if (ie) {
			//alert("editor:" + sel.createRange().parentElement().tagName);
			return sel.createRange();
		}
		else {
			if (sel && sel.rangeCount>0)
				return sel.getRangeAt(0);
			else return editor.doc.createRange();
		}		
	}
	
	this.getRange=function(){
		return _createRange(_getSelection());
	}
	this.setRange=function(range){
		if(ie)
			range.select();
		else{
			var sel = _getSelection();
			sel.removeAllRanges()
			if(range)
				sel.addRange(range);
		}
	}
	
	function _getNearestTag(ptag, tagNameToFind){
		while(ptag && ptag.tagName!="BODY" && ptag!=editor.editelement && ptag.tagName!=tagNameToFind){
			ptag=ptag.parentNode;			
		}
		if (ptag && ptag.tagName==tagNameToFind && ptag!=editor.editelement)
			return ptag;
		else return null;
	}
		
	function _getNearestTagFromSelection(tagNameToFind){
		var range = _createRange(_getSelection());
		if (ie){
			try{
				return _getNearestTag(range.parentElement(), tagNameToFind);
			} catch (e){
				return _getNearestTag(range.item(0), tagNameToFind);
			}
		}
		else {
			var startNode=range.startContainer;
			if (startNode.nodeType==1)		// Element
				return _getNearestTag(startNode.childNodes[range.startOffset]||startNode, tagNameToFind);
			else // Textnode or other nodes ...
				return _getNearestTag(startNode.parentNode, tagNameToFind);
		}
	}
	
	this.getNearestTagFromSelection=function(tagNameToFind){
		return _getNearestTagFromSelection(tagNameToFind.toUpperCase());
	}

	this.getNearestTagsFromSelection=function(tagNameToFind){
		if(ie)
			return [this.getNearestTagFromSelection(tagNameToFind)]
		else{	// standard selection/range support
			var sel = _getSelection();
			var c = sel.rangeCount;
			var nodes = [];
			for(i=0; i<c; i++){
				var range = sel.getRangeAt(i);
				var node;
				var startNode=range.startContainer;
				if (startNode.nodeType==1)		// Element
					node = _getNearestTag(startNode.childNodes[range.startOffset], tagNameToFind.toUpperCase());
				else // Textnode or other nodes ...
					node = _getNearestTag(startNode.parentNode, tagNameToFind.toUpperCase());
				if(node)
					nodes.push(node);
			}
			return nodes; 
		}
	}
	
	this.getParentNodeFromSelection=function(){
		var range = _createRange(_getSelection());
		if (ie){
			try{
				return range.parentElement();
			} catch (e){
				return range(0);
			}
		}
		else {
			var startNode=range.startContainer;
			if (startNode.nodeType==1){		// Element
				try{
					return startNode.childNodes[range.startOffset] || startNode;	// don't understand this but it works so far.
				}
				catch(e){
					return startNode
				}
			}
			else // Textnode or other nodes ...
				return startNode.parentNode;
		}
	}
	this.getPathFromSelection=function(){
		var path = [];
		try{
			var ptag = this.getParentNodeFromSelection();
			while(ptag && ptag.tagName!="BODY" && ptag!=editor.editelement){
				path.push(ptag);
				ptag=ptag.parentNode;			
			}
		} catch(e){};
		return path;
	}
	
	this.isEditorSelected = function(){
		try{
			var ptag = this.getParentNodeFromSelection();
			while(ptag && ptag.tagName!="BODY"){
				if(ptag==this.spanElement)
					return true;
				ptag=ptag.parentNode;			
			}
		} catch(e){
			//console.log(e);
		};
		return false;
	}
	
	function _getParentTagFromSelection(){
		var range = _createRange(_getSelection());
		if (ie)
			return range.parentElement();
		else return range.startContainer.parentNode;
	}

	function insertNodeAtSelection(node){
		var range = _createRange(_getSelection());
		if(ie)
			try{
				range.pasteHTML(node.outerHTML);				
			} catch(e){
				range(0).outerHTML=node.outerHTML;
			}
		else{
			range.deleteContents();	// deletes the selected Text
			range.insertNode(node);
		}
	}
	
	function getSelectedText(){
		if(ie)
			return _createRange(_getSelection()).text;
		else 
			return _getSelection().toString();
	}
	this.getSelectedText=getSelectedText;
	
	this.getHTML=function(){
		var ret="";
		if (editor.viewmode=="html"){			
			ret=editor.sourcecodeElement.value;
			if(ie && ret.substr(0, 7).toLowerCase()=="<script")
				ret = "<br>\n" + ret; 
		}
		else ret= editor.iframe ? editor.doc.body.innerHTML : editor.editelement.innerHTML;
		return ret;
	}

	this.getText=function(){		
		if(ie)
			return editor.editelement.innerText;
		else{
			var range=this.getRange();
			range.selectNode(editor.iframe ? editor.doc.body : editor.editelement);
			return range.toString();
		}
	}
	
	this.setHTML=function(html){
		if(ie){
			if(html.substr(0, 7).toLowerCase()=="<script")
				html = "<br>\n" + html;		 	
		}
		(this.iframe ? this.doc.body : this.editelement).innerHTML = html;
	}
		
	function _updateToolbar()
	{
		if (editor.toolbar)
			editor.toolbar.update();
	}
	
	this.execCmd=function(cmd, param){

		/*
		if(!this.isEditorSelected())
			return;
		*/

		switch(cmd) {
			case "Paste":
				if (ie){	// paste unformatted text. Use Ctrl-V to paste formatted text.
					var _data=window.clipboardData.getData("text");
					window.clipboardData.setData("Text", _data);
					editor.doc.execCommand(cmd, false, null);
					_updateToolbar();
				}
				else editor.doc.execCommand(cmd, false, null);
				break;
   
   			case "Sup":
   				insertSup();
   				break;
   			case "Sub":
   				insertSub();
   				break;
   
		    case "Undo":
		    case "Redo":
		    case "RemoveFormat":
		    case "Bold":
			case "Italic":
			case "Underline":
			case "Indent":
			case "Outdent":
			case "JustifyLeft":
			case "JustifyCenter":
			case "JustifyRight":
			case "JustifyFull":
			case "RemoveFormat":
				editor.doc.execCommand(cmd, false, null);
				_updateToolbar();
				break;
		
			case "InsertUnorderedList":
			case "InsertOrderedList":
				editor.doc.execCommand(cmd, false, null);
				var el = this.getParagraph();
				if(el && !WGA.isIE){
					this.selection.save();
					this.removeNode(el);
					this.selection.restore();
				}
				_updateToolbar();
				break;

			case "Unlink":
				this.removeLink();
				break;
			
			case "FontName":
			case "FontSize":
			case "ForeColor":
			case "BackColor":
				editor.doc.execCommand(cmd, false, param);
				_updateToolbar()
				break;
			case "FormatBlock":
				if (ie || WGA.isTrident)
					param="<"+param+">";	// IE needs <> around the tags!
				try{
					editor.doc.execCommand(cmd.toLowerCase(), false, param);
				}
				catch(e){
					//console.log(e);
				}
				_updateToolbar()
			break;
			
			case "InsertImg":
				insertImg(param);
				break;
			case "InsertLink":
				insertLink(param);
				break;
			case "InsertTable":
				insertTable(param);
				break;
			case "InsertTableRow":		
				insertTableRow();				
				break;
			case "DeleteTableRow":		
				deleteTableRow();				
				break;
			case "InsertTableCol":
				insertTableCol();				
				break;
			case "DeleteTableCol":		
				deleteTableCol();				
				break;
			case "MergeTableCells":		
				mergeTableCells();
				break;
			case "SplitTableCell":		
				splitTableCell();				
				break;
			
			default:
				alert("Unknown Command: " + cmd);
				break;
		}
	}	

	function insertSub(){		
		var txt = getSelectedText();
		var el=editor.doc.createElement("sub");
		el.innerHTML=txt;
		insertNodeAtSelection(el);			
	}
	function insertSup(){		
		var txt = getSelectedText();
		var el=editor.doc.createElement("sup");
		el.innerHTML=txt;
		insertNodeAtSelection(el);			
	}
	
	this.getImgURL=function(){
    	//setFocus();
		var tag=_getNearestTagFromSelection("IMG");    	
		if (tag)
			return tag.src;
		else return "";
	}	

	this.createImg=function(url, urltype){
		// create img  element	
		if(!this.isEditorSelected())
			return alert("Editor not active")
		var imgTag=_getNearestTagFromSelection("IMG");
		if (!imgTag){
			if (ie){
		    	var range = _createRange(_getSelection());    	
				range.execCommand("insertImage", false, url);
				// find img-tag:
				imgTag=_getNearestTagFromSelection("IMG");
			}
			else{
				imgTag=editor.doc.createElement("img");
				imgTag.src=url;
				insertNodeAtSelection(imgTag);
			}
		}
		else imgTag.src=url;
		imgTag.removeAttribute("width");
		imgTag.removeAttribute("height");
		imgTag.title="";
		imgTag.alt = WGA.util.decodeURI(imgTag.src.split("/").pop().split("?")[0]);
		
		if(urltype){
			var c = imgTag.className;
			c = c.replace(/wga-urltype-\w+ */, "");
			imgTag.className = c + (c?" ":"") + "wga-urltype-" + urltype;
		}
		return imgTag;
	}
	
	this.getLinkURL=function(){
    	//setFocus();
		var aTag=_getNearestTagFromSelection("A");    	
		if (aTag){
			// inside an A-Tag:
			return aTag.href;
		}
		else return "";
	}
	
	this.createLink=function(url, linktext, urltype){
		if(!this.isEditorSelected())
			return alert("Editor not active")
		var aTag=_getNearestTagFromSelection("A");    	
		if (!aTag){
	    	var range = _createRange(_getSelection());    	
			if (ie){
				var fake_url = "#"+new Date().getTime().toString();
				range.execCommand("CreateLink", false, fake_url);
				// find a-tag:
				var links = editor.editelement.getElementsByTagName("a");
				for(var i=0; i<links.length; i++){
					if(links[i].href.indexOf(fake_url)>=0){
						if(aTag)
							this.removeLink(links[i])	// IE creates one link per <p>. Remove all except the first.
						else aTag=links[i];
					}
				}
				if(!aTag){
					return alert("Unable to insert Link: " + url + ", " + linktext + ", " + urltype);
				}
				
		   	   	if (linktext!=undefined){
					if (aTag && (!aTag.innerHTML || aTag.innerHTML==""))
						aTag.innerText=linktext;
				}
			}
			else{	// Mozilla:
				aTag=editor.doc.createElement("a");
				var contents=range.cloneContents();
				if (linktext!=undefined){
					// Test, if selection is empty (not so easy as expected!!):
			    	if (contents.childNodes.length==0 || (contents.childNodes.length==1 && contents.firstChild.nodeValue=="")){
						// selection is empty: write Linktext into the link
			    		aTag.appendChild(editor.doc.createTextNode(linktext));
					}
				}
				aTag.appendChild(range.extractContents());
				range.insertNode(aTag);									
			}
		}
		aTag.href=url;
		if(urltype){
			var c = aTag.className;
			c = c.replace(/wga-urltype-\w+ */, "");
			aTag.className = c + (c?" ":"") + "wga-urltype-" + urltype;
		}
		
		return aTag;
	}
	
	this.removeLink=function(aTag){
		if(!aTag)
			aTag=_getNearestTagFromSelection("A");	// try to find one
		if(aTag){
			// the following works in FF and Safari3 and IE6
			// editor.doc.execCommand("unlink") is currently not supportet in Safari
			var parent = aTag.parentNode;
			var children = aTag.childNodes;
			var nodelist = [];
			for(var i=0; i<children.length; i++)
				nodelist.push(children.item(i));
			for(var i=0; i<nodelist.length; i++){
				var node=nodelist[i];
				//console.log(i, node, node.nodeType, node.nodeName, node.NodeValue);
				parent.insertBefore(node, aTag)
			}
			parent.removeChild(aTag);
		}
	}
	
	this.isInTable=function(){
		if (_getNearestTagFromSelection("TABLE"))
			return true;
		else return false;		
	}
	
	this.setClassName=function(tag, newclass){
		var t_el=_getNearestTagFromSelection(tag.toUpperCase());
		if (t_el)
			t_el.className=newclass;
		else alert("setClassName: Element " + tag + " not found");
	}
	
	this.setTrClassName=function(newclass, applyto){
		if (applyto=="selected")
			this.setClassName("tr", newclass);
		else if(applyto=="all"){
			var el=_getNearestTagFromSelection("TABLE");
			if (!el) return;
			trlist=el.getElementsByTagName("TR");
			for (var i in trlist)
				trlist[i].className=newclass;
		}
	}
	this.setTdClassName=function(newclass, applyto){
		if (applyto=="selected")
			this.setClassName("td", newclass);
		else if(applyto=="row"){
			var el=_getNearestTagFromSelection("TR");
			if (!el) return;
			tdlist=el.getElementsByTagName("TD");
			for (var i in tdlist)
				tdlist[i].className=newclass;
		}
		else if(applyto=="all"){
			var el=_getNearestTagFromSelection("TABLE");
			if (!el) return;
			var tdlist=el.getElementsByTagName("TD");
			for (var j in tdlist)
				tdlist[j].className=newclass;
		}
	}
	
	function insertTable(params){
		return this.createTable(params.rows, params.cols, params.width, params.align)
	}
	this.createTable=function(rows, cols, width){
		rows=parseInt(rows);
		cols=parseInt(cols);
		if (rows.toString()=="NaN" || cols.toString=="NaN"){
			alert("please enter a number for #rows and #cols");
			return;
		}
		//alert(rows + "/" + cols);
		var el_table=editor.doc.createElement("table");
		el_table.style.width=width||"100%"; 
		//el_table.align=align||"";
		var el_tbody=editor.doc.createElement("tbody");
		el_table.appendChild(el_tbody);
		
		for (r=0; r < rows; r++){
			el_tr=editor.doc.createElement("tr");
			el_tbody.appendChild(el_tr); 			
			for (c=0 ; c < cols; c++ ){
				el_td=editor.doc.createElement("td");
				if (!ie)
					// Mozilla needs a <br>
					el_td.appendChild(editor.doc.createElement("br"));
				el_tr.appendChild(el_td);
			}
		}			
		insertNodeAtSelection(el_table);
		_updateToolbar();
		return el_table;
	}
	
	function insertTableRow(){
		//setFocus();
		if (!editor.isInTable()){
			alert("Please select a table");
			return;
		}

		var tr = _getNearestTagFromSelection("TR")		
		if (!tr) {
			return alert("no tr found");
		}
		var otr = tr.cloneNode(true);
		//clear row
		var tds = otr.getElementsByTagName("td");
		for (var i in tds) {
			var td = tds[i];
			try{
				td.rowspan = 1;
				td.innerHTML = ie ? "" : "<br>";
			}
			catch(e){
				// may happen, if td contains a sub-table. Ignore this.
			}
		}
		tr.parentNode.insertBefore(otr, tr.nextSibling);
		_updateToolbar();
	}

	function deleteTableRow(){
		//alert("deleteTableRow");
		if (!editor.isInTable()){
			alert("Please select a table");
			return;
		}
		
		var tr=_getNearestTagFromSelection("TR");
		var table=_getNearestTagFromSelection("TABLE");
		tr.parentNode.removeChild(tr);
		if (table.rows.length==0){
			// last row: delete the table.
			table.parentNode.removeChild(table);			
		}
		_updateToolbar();
	}

	function insertTableCol(){
		//alert("insertTableCol");
		if (!editor.isInTable()){
			alert("Please select a table");
			return;
		}
		var td=_getNearestTagFromSelection("TD");
		var index = td.cellIndex+1;
		var rows=td.parentNode.parentNode.rows;
		for (i=0; i<rows.length; i++){
			tr=rows[i]
			if (tr.cells.length>=index)
				var new_td=tr.insertCell(index);	
			else
				var new_td=tr.insertCell();
			new_td.setAttribute("valign", "top");
			if (!ie)
				new_td.innerHTML="<br>";
		}
		_updateToolbar();
	}

	function deleteTableCol(){
		var td=_getNearestTagFromSelection("TD");
		if (!td){
			alert("Plese select one single cell");
			return;
		}
		var index= td.cellIndex;
		var rows=td.parentNode.parentNode.rows;
		for (i=0; i<rows.length; i++){
			tr=rows[i];
			if (tr.cells.length<index+1)
				index=tr.cells.length-1;	// take the last cell-index instead
			toBeDeleted_td=tr.cells[index];
			if (toBeDeleted_td.colSpan>1)
				toBeDeleted_td.colSpan=toBeDeleted_td.colSpan-1
			else{
				tr.deleteCell(index);
				if (tr.cells.length==0){
					// last cell: delete the table.
					table=_getNearestTag(tr, "TABLE");
					table.parentNode.removeChild(table);
					break;
				}
			}
		}
		_updateToolbar();
	}

	this.mayMergeTableCells=function (){
		var td=_getNearestTagFromSelection("TD");
		if (!td)
			return false;			// not in a table cell
		var index=td.cellIndex;
		var tr=td.parentNode;
		if (tr.cells[index+1])
			return true;			// cells to the right
		else return false;			// we are in the right cell
	}
	
	function mergeTableCells(){
		var td=_getNearestTagFromSelection("TD");
		if (!td){
			alert("Plese select one single cell");
			return;
		}
		var index= td.cellIndex;
		var tr=td.parentNode;
		if (tr.cells[index+1]){
			td.colSpan = td.colSpan + tr.cells[index+1].colSpan;
			tr.deleteCell(index+1);
			_updateToolbar();
		}
		else
			alert("Please set cursor into a table-cell that has a column to the right");			
	}

	this.maySplitTableCell=function(){
		var td=_getNearestTagFromSelection("TD");
		if (!td)
			return false;			// not in a table cell
		var index= td.cellIndex;
		var tr=td.parentNode;
		if (td && td.colSpan>1) return true;
		else return false;
	}
	
	function splitTableCell(){
		var td=_getNearestTagFromSelection("TD");
		if (!td)
			return false;			// not in a table cell
		var index= td.cellIndex;
		var tr=td.parentNode;
		if (td && td.colSpan>1){
			td.colSpan=td.colSpan-1;
			var new_td=tr.insertCell(index+1);
			new_td.setAttribute("valign", "top");
			if (!ie)
				new_td.innerHTML="<br>";
			_updateToolbar();
		}
		else
			alert("Split not possible in this cell.");
	}

	this.insertHTML=function (theHTML){
		var range = editor.getRange();
		var el=editor.doc.createElement("span");
		el.innerHTML=theHTML;
		insertNodeAtSelection(el);

		// Set Cursor after inserted HTML:
		var nextSibling = el.nextSibling;		// set cursor to this position later
		
		/*
		 * We have to clean up the DOM to avoid nested block-nodes:
		 * This is mainly an "intelligent copy" of DOM nodes.
		 * We create a new paragraph node and copy eveything to it
		 * If we find a block node in inserted HTML, we move it AFTER the new paragraph. "closing" the new paragraph
		 * We remove the original <p> afterwards
		 */
		var p = this.getParagraph();
		if(p){
			var parentsOfSpan=[];
			var parentOfSpan=el.parentNode;
			while(parentOfSpan && parentOfSpan!=p){
				parentsOfSpan.push(parentOfSpan)
				parentOfSpan = parentOfSpan.parentNode
			}
			function isParentOfSpan(node){
				for(var i=0; i<parentsOfSpan.length; i++)
					if(node==parentsOfSpan[i])
						return true;
				return false;
			}
			
			var copyTo = document.createElement(p.tagName);
			p.parentNode.insertBefore(copyTo, p);

			copyAndSplitNode(p)

			p.parentNode.removeChild(p);
		}
		else this.removeNode(el)

		// set cursor after interted HTML
		if(nextSibling){
			range.selectNodeContents(nextSibling);
			range.collapse(true);
			editor.setRange(range);
		}
		
		function copyAndSplitNode(node){

			var blocktags = "p,h1,h2,h3,h4,h5,h6";
			var children = node.childNodes;
			for(var i=0; i<children.length; i++){
				var node = children[i];
				if(node==el){
					// this is the pasted <span> Tag. Move it childnodes to "newp"
					//console.log("found pasted <span>", el.innerHTML);
					var haveBlockTag = false;
					var pastedNodes = el.childNodes;
					for(var j=0; j<pastedNodes.length; j++){
						var node = pastedNodes[j];
						//console	.log("pasted node", node, haveBlockTag);
						var tagName = node.tagName;
						var isBlockTag = (tagName && blocktags.indexOf(tagName.toLowerCase())>=0) 
						if(isBlockTag){
							// we have a block tag -> move it and make it to the new "newp" (=destination to move nodes)
							haveBlockTag = true;
							p.parentNode.insertBefore(node, p);	// move node
						}
						else {
							if(haveBlockTag){
								if(!copyTo.innerHTML)
									copyTo.parentNode.removeChild(copyTo);
								copyTo = document.createElement(p.tagName);
								p.parentNode.insertBefore(copyTo, p);
								haveBlockTag = false;
							}
							copyTo.appendChild(node);
						}

						j--;
					}
					
					if(haveBlockTag){
						if(!copyTo.innerHTML)
							copyTo.parentNode.removeChild(copyTo);
						copyTo = document.createElement(p.tagName);
						p.parentNode.insertBefore(copyTo, p);
					}
					
				}
				else if (isParentOfSpan(node)){
					var newnode = document.createElement(node.tagName);
					for(a=0; a<node.attributes.length; a++){
						var name =  node.attributes[a].name;
						var value =  node.attributes[a].value;
						newnode.setAttribute(name, value);
					} 
					copyTo.appendChild(newnode);
					copyTo = newnode;
					//console.log("created new " + node.tagName);
					copyAndSplitNode(node);
				}
				else{
					copyTo.appendChild(node);
					i--;
				}
			}
			if(!copyTo.innerHTML)
				copyTo.parentNode.removeChild(copyTo);
		}
		
	}

	this.getCurrentSelection = function(){
		var range = this.getRange();
		return {
			sc: range.startContainer,
			so: range.startOffset,
			ec: range.endContainer,
			eo: range.endOffset,
			startNode: range.startContainer.nodeType==3 /* text node */ ? range.startContainer : range.startContainer.childNodes[range.startOffset],
			endNode: range.endContainer.nodeType==3 /* text node */ ? range.endContainer : range.endContainer.childNodes[range.endOffset]
		}
	}
	this.setCurrentSelection = function(pos){
		var range = this.getRange();
		range.setStart(pos.sc, pos.so)
		range.setEnd(pos.ec, pos.eo)
		this.setRange(range);
	}
	
	this.insertSection = function(){
		var div=editor.doc.createElement("div");
		var range = this.getRange();

		var para = this.getParagraph();
		if(para==null){
			para = editor.doc.createElement("p");
			range.surroundContents(para);
			if(range.toString()=="")
				para.appendChild(editor.doc.createElement("br"));
		}
		range.selectNode(para);
		range.surroundContents(div);
	}
	
	this.removeNode = function(node, add_br){
		if(add_br){
			var br=editor.doc.createElement("br");
			node.appendChild(br);
		}
		var range = this.getRange();
		range.selectNodeContents(node);
		var parent = node.parentNode;
		var df = range.extractContents();	// document fragment
		parent.insertBefore(df, node);
		parent.removeChild(node);
	}

	this.selection = function(){
		
		var sel;
		
		return{
			save: function(){
				var range = editor.getRange();
				sel = {
					sc: range.startContainer,
					so: range.startOffset,
					ec: range.endContainer,
					eo: range.endOffset
				}
				//console.log("saved selection", sel.sc, sel.so);
			}
			,restore: function(){
				if(!sel){
					return;
				}
				try{
					editor.doc.defaultView.focus();
					var range = editor.getRange();
					range.setStart(sel.sc, sel.so)
					range.setEnd(sel.ec, sel.eo)
					editor.setRange(range);
				}
				catch(e){}
				sel = null;
			}
			,restoreStart: function(){
				if(!sel)
					return;
				try{
					//console.log("try restoreStart", sel.sc, sel.so); 
					editor.doc.defaultView.focus();
					var range = editor.getRange();
					range.setStart(sel.sc, sel.so)
					editor.setRange(range);
					//console.log("Restored Start", sel.sc, sel.so);
				}
				catch(e){
					console.log("unable to restoreStart", e);
				}
				sel = null;
			}

			,clean: function(){
				var node = this.expand().cloneContents();
				var html = AFW.RTF.getCleanInnerHTML(node);
				editor.insertHTML(html);
				editor.doc.defaultView.focus();
			}

			,selectNodeContents: function(node){
				editor.focus();				
				var range = editor.getRange();
				range.selectNodeContents(node);
				editor.setRange(range);
			}
			,expand: function(){
				var range = editor.getRange();
				var startNode = (range.startContainer.nodeType==3 /* Text Node */ ? range.startContainer : range.startContainer.childNodes[range.startOffset]); 
				var endNode = (range.endContainer.nodeType==3 /* Text Node */ ? range.endContainer : range.endContainer.childNodes[range.endOffset]);
				var p = editor.getParagraph(startNode);		//_getNearestTag(startNode, "P")
				if(p)
					range.setStartBefore(p); 
				var p = editor.getParagraph(endNode);		//_getNearestTag(endNode, "P")
				if(p)
					range.setEndAfter(p);
				editor.setRange(range);
				return range;
			}
			,surround: function(tagName){
				var range = this.expand();
				var el = editor.doc.createElement(tagName)
				range.surroundContents(el);
				range.selectNodeContents(el);
				editor.setRange(range);
			}
			,focus: function(){
				var range = editor.getRange();
				editor.focus();
				editor.setRange(range);			
			}
		}
	
	}()


	this.closeEditor=function(){
		this.stopResizeIframeTimer();
		if(this._timerToolbar)
			window.clearTimeout(this._timerToolbar);
		if(this.toolbar && this.toolbar.setEditor){
			this.toolbar.setEditor(null);
			this.toolbar = null;
		}
		unregisterEvents();
		if (this.toolbar && this.toolbar.hide)
			this.toolbar.hide(true);

		// restore original state
		this.spanElement.parentNode.removeChild(this.spanElement);
		this.sourcecodeElement.parentNode.removeChild(this.sourcecodeElement);
		pasteIframe.element.parentNode.removeChild(pasteIframe.element);
		this.orgElement.style.display="block";
	}


	function createPasteIframe(){
		var doc;
		var pasteIframe = document.createElement("iframe");
		
		if(WGA.isIE)
			pasteIframe.style.position="absolute";
		else pasteIframe.style.position="fixed";

		pasteIframe.style.top="0px";
		pasteIframe.style.left="-10000px";
		pasteIframe.style.width="200px";
		
		/*
		// for debugging:
		pasteIframe.style.left="10px";
		pasteIframe.style.top="100px";
		pasteIframe.style.width="200px";
		pasteIframe.style.background="white";
		*/

		pasteIframe.onload=function(){
			//console.log("paste iframe loaded");
			if(WGA.isIE)
				doc = pasteIframe.contentWindow.document;
			else doc = pasteIframe.contentDocument;
			doc.designMode="on";
		}
		
		function doPaste(){
			var range = editor.getRange();
			doc.body.innerHTML="";
			pasteIframe.contentWindow.focus();

			window.setTimeout(function(){
				var pasted_body = doc.body;
				var html = AFW.RTF.getCleanInnerHTML(doc.body, editor.toolbar==null)
				if(html)
					editor.insertHTML(html);
					
				var range = editor.getRange();
				editor.doc.defaultView.focus();
				editor.setRange(range);
				
			}, 10)
		}
		
		return{
			element: pasteIframe,
			doPaste: doPaste
		}
	}

}

AFW.RTF.getCleanInnerHTML = function(node, isTextBlock){

	//console.log("cleanHTML", node, node.childNodes);

	var good_els = "#h1#h2#h3#h4#h5#h6#a#img#p#pre#br#ul#ol#li#blockquote#div#table#tbody#tr#td#b#i#u#sub#sup#";
	var good_els_textblock = "#br#p#";
	var bad_els = "#head#script#style#"

	var el = document.createElement("div");
	var children = node.childNodes;
	for(var i=0; i<children.length; i++){
		copyElement(children[i], el, isTextBlock?good_els_textblock:good_els);
	}
	return el.innerHTML;

	function hasToolbarClass(classes, cls){
		for(var i=0; i<classes.length; i++){
			var c = classes[i].split("|");
			if(c[1]==cls)
				return true;
		}
		return false;
	}

	function copyElement(source, dest, good_els){

		if(source.nodeType==source.TEXT_NODE){				// text node 
			var txt = source.nodeValue.replace(/\xA0/g, " ");	// removes &nbsp;
			dest.appendChild(document.createTextNode(txt));
		}
		else if(source.nodeType==source.ELEMENT_NODE || source.nodeType==source.DOCUMENT_FRAGMENT_NODE){		// dom element

			if(source.style && source.style.display=="none")
				return;						// don't copy hidden elements

			var el = dest;
			var tagname=source.nodeName.toLowerCase();
			
			/*
			 * filter good and bad tags
			 */
			if(bad_els.indexOf("#"+tagname+"#")>=0){
				//console.log("ignoring bag tag " + tagname);
				return;		// we don't want this tag and it's contents
			}
			else if(good_els.indexOf("#"+tagname+"#")>=0){
				
				/* special handling for pasted images in Safari: */
				if(tagname=="img" && source.src.indexOf("webkit-fake-url://")==0)
					return;		// image is useless so ignore this image
				
				/* 
				 * special tags attribute handling 
				 */
				var attributes=[];
				switch(tagname){
					case "a":
						attributes = ["href"]
						break;
					case "img":
						attributes = ["src", "width", "height", "title", "alt", "data-wga-urlinfo", "wga:urlinfo"];
						break;
					case "table":
						attributes = ["width"]
						break;
					case "td":
						attributes = ["colspan", "rowspan", "width"]
						break;
					case "div":
						attributes = ["align"]
					case "p":
					case "pre":
					case "h1":
					case "h2":
					case "h3":
					case "h4":
					case "h5":
					case "h6":
						if(!source.innerHTML.trim())
							return;		// ignore empty block elements
						break;
				}

				/* create element */
				el = dest.appendChild(document.createElement(tagname));

				// copy attributes
				for(var i=0; i<attributes.length; i++){
					var attribute = attributes[i];
					var val = source.getAttribute(attribute);
					if(val)
						el.setAttribute(attribute, val);
				}
				//console.log("good tag " + tagname, attributes);
				
				/*
				 * special handling for wga-urltype classes
				 */
				var cn = source.className.split(" ");
				var wga_classes=[];
				for(var i=0; i<cn.length; i++){
					var cls = cn[i];
					if(!cls)
						continue;
					if(cn[i] && cn[i].indexOf("wga-urltype")==0){
						// this is a WGA-URL. We need to check 
						// 1) if this is on the same server
						// 2) if the URL is a file url that points the the same content
						
						var urlinfo = AFW.RTF.getURLInfo(source);
						if(urlinfo.domain != document.domain)
							continue;
						
						if(cls=="wga-urltype-intfile"){
							var container = urlinfo.path[urlinfo.path.length-2]
							if(container!=editor.contentkey){
								cls = "wga-urltype-extfile";		// better use extfile because we don't know the destination
								urlinfo.type="extfile";
								urlinfo.key = container+"/"+urlinfo.key;
							}
						}
						wga_classes.push(cls);
						AFW.RTF.setURLInfo(el, {type:urlinfo.type, key:urlinfo.key})
					}
					else if(editor.toolbar){
						if((tagname=="a" && hasToolbarClass(editor.toolbar.linkStyleList, cls))
							|| (tagname=="div" && hasToolbarClass(editor.toolbar.sectionStyleList, cls))
							|| (tagname=="p" && hasToolbarClass(editor.toolbar.paragraphStyleList, cls))
							|| (tagname=="table" && hasToolbarClass(editor.toolbar.tableStyleList, cls))
							|| (tagname=="tr" && hasToolbarClass(editor.toolbar.trStyleList, cls))
							|| (tagname=="td" && hasToolbarClass(editor.toolbar.tdStyleList, cls))
							|| (tagname=="img" && hasToolbarClass(editor.toolbar.imageStyleList, cls))
						)
							wga_classes.push(cls); 
					}
				}
				if(wga_classes.length)
					el.className = wga_classes.join(" ");
				
			}
			// else console.log("filtering: " + tagname);	// ignore this tag but copy it's contents
			
			/*
			 * copy child nodes
			 */
			var children = source.childNodes;
			for(var i=0; i<children.length; i++){
				copyElement(children[i], el, good_els);
			}
			
		}
	}
}

// old RTF Interface:
AFW.RTFEditor = AFW.RTF.editor
