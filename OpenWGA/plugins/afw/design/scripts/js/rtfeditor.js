/*
 *	RTF Editor
 *
 *	This script is part of the OpenWGA CMS plattform.
 *	(c) Innovation Gate
 */

define(["jquery"], function($){
	
	function editor(el){
		var editor = this;
		
		this.el = $(el)
		this.editelement = this.el[0];
		this.doc = this.editelement.ownerDocument;
		
		this.el.prop("contenteditable", true);
		this.el.focus();

		this.doc.execCommand("styleWithCSS", false, true);
		this.doc.execCommand("defaultParagraphSeparator", false, "p");
		
		this.el.on({
			"keydown": function(ev){
				//console.log("keypress", ev.key, ev.which);
				if(ev.key=="Tab"){
					var para = editor.getParagraph();
					if(para && para.tagName!="PRE"){
						if(ev.shiftKey)
							editor.execCmd("Outdent");
						else editor.execCmd("Indent");
					}
					else editor.insertHTML("\t")
					ev.preventDefault();
				}
			},
			"keyup focus click": function(ev){
				if(editor.toolbar)
					setTimeout(editor.toolbar.update, 100);
			},
			"mousedown click": function(ev){
				//console.log(ev.target, ev.target.tagName)
				// Safari/chrome/edge don't select images
				if(ev.target && ev.target.tagName=="IMG"){			
					var imgel = ev.target;
					var sel = editor.getSelection();
					var range = editor.doc.createRange()
					range.setStartBefore(imgel);
					range.setEndAfter(imgel);
					sel.removeAllRanges()
					sel.addRange(range);
				}
			},
			"paste": function(ev){
				if(ev.originalEvent.clipboardData){
					var dt = ev.originalEvent.clipboardData
					//console.log("types", dt.types);
					
					var html = dt.getData("text/html")
					var plain = dt.getData("text/plain")

					if(html){
						var html = getCleanHTML(html, editor.toolbar)
						if(html){
							editor.insertHTML(html);
							ev.preventDefault();
						}
					}
					else if (plain) {
						plain = plain.replace(/&/g, '&amp;')
							.replace(/</g, '&lt;')
							.replace(/>/g, '&gt;')
							.replace(/\n/g, "<br>")
							.replace(/\xA0/g, " ");	// removes &nbsp;
						editor.insertHTML(plain);
						ev.preventDefault();
					}
				}

			}
		})
		
	}

	editor.prototype={
		html: function(html){
			return arguments.length ? this.el.html(html) : this.el.html()
		},
		close: function(){
			this.el.off()
			this.el.removeAttr("contenteditable");
		},
		getRange: function(){
			var sel = this.doc.getSelection()
			/*
			if(!sel.rangeCount){
				console.log("rangecount is 0")
				sel = window.getSelection()
			}
			*/
			return sel.getRangeAt(0)
		},
		focus: function(){
			this.el.focus();
		},
		getSelection: function(){
			return this.doc.getSelection();
		},
		getSelectedText: function(){
			return this.getSelection().toString();
		},
		queryCommandState: function(state){
			return this.doc.queryCommandState(state)
		},
		setDefaultParagraphSeparator: function(el){
			this.doc.execCommand("defaultParagraphSeparator", false, el);
		}
	}

	editor.prototype.setRange=function(range){
		var sel = this.getSelection();
		sel.removeAllRanges()
		if(range)
			sel.addRange(range);
	}

	editor.prototype.insertHTML = function(html){
		var range = this.getRange();
		range.deleteContents();
        var el = document.createElement("div");
        el.innerHTML = html;
        var frag = document.createDocumentFragment(), 
        	node;
        while ( (node = el.firstChild) ) {
            frag.appendChild(node);
        }
        range.insertNode(frag);
        range.collapse(false);
        this.setRange(range);
	}
	
	editor.prototype.execCmd = function(cmd, param){
		this.doc.execCommand(cmd, false, param);
		if(this.toolbar)
			this.toolbar.update();
	}
	
	editor.prototype.getParagraph = function(el){
		var para_tags = "P,H1,H2,H3,H4,H5,H6,PRE";
		try{
			var ptag = el || getParentNodeFromSelection(this.getRange());
			while(ptag && ptag.tagName!="BODY" && ptag.tagName!="TD" && ptag.tagName!="BLOCKQUOTE" && ptag!=this.editelement){
				if(ptag.nodeType==ptag.ELEMENT_NODE && para_tags.indexOf(ptag.tagName)>=0)
					return ptag;
				ptag=ptag.parentNode;			
			}
		} catch(e){
			console.log("getParagraph", e);			
		};
		return null;

		// helper
		function getParentNodeFromSelection(range){
			var startNode=range.startContainer;
			if (startNode.nodeType==startNode.ELEMENT_NODE)		// Element
				return startNode.childNodes[range.startOffset] || startNode;
				//return startNode;
			return startNode.parentNode;		// Textnode or other node types ...
		}

	}
	
	editor.prototype.getNearestTagFromSelection = function(tagNameToFind){
		tagNameToFind = tagNameToFind.toUpperCase();
		var range = this.getRange();
		var startNode=range.startContainer;
		if (startNode.nodeType==startNode.ELEMENT_NODE)		// Element
			return getNearestTag(this, startNode.childNodes[range.startOffset]||startNode, tagNameToFind);
			//return getNearestTag(this, startNode, tagNameToFind);
		return getNearestTag(this, startNode.parentNode, tagNameToFind);	// Textnode or other node types ...

		// helper
		function getNearestTag(editor, ptag, tagNameToFind){
			while(ptag && ptag.tagName!="BODY" && ptag!=editor.editelement && ptag.tagName!=tagNameToFind){
				ptag=ptag.parentNode;			
			}
			if (ptag && ptag.tagName==tagNameToFind && ptag!=editor.editelement)
				return ptag;
			else return null;
		}
	
	}

	editor.prototype.createImg=function(url, urltype){
		var imgTag=this.getNearestTagFromSelection("IMG");
		if (!imgTag){
			imgTag=this.doc.createElement("img");
			imgTag.title = imgTag.alt = decodeURI(url.split("/").pop().split("?")[0]);
			var range = this.getRange();
			range.deleteContents();	// deletes the selected Text
			range.insertNode(imgTag);
		}
		imgTag.src=url;
		
		return imgTag;
	}

	editor.prototype.createLink=function(url, linktext, urltype){
		var aTag=this.getNearestTagFromSelection("A");    	
		if (!aTag){
			linktext = linktext || url;
	    	var range = this.getRange();
			aTag=this.doc.createElement("a");
			if(this.getSelection().isCollapsed)
				aTag.innerHTML=linktext;
			else aTag.appendChild(range.extractContents());
			range.insertNode(aTag);
			range.collapse(false);
		}
		aTag.href=url;

		return aTag;
	}

	editor.prototype.removeNode = function(node, add_br){
		if(add_br){
			var br=this.doc.createElement("br");
			node.appendChild(br);
		}
		var range = this.getRange();
		range.selectNodeContents(node);
		var parent = node.parentNode;
		var df = range.extractContents();	// document fragment
		parent.insertBefore(df, node);
		parent.removeChild(node);
	}
	
	editor.prototype.removeLink = function(aTag){
		if(!aTag)
			aTag=this.getNearestTagFromSelection("A");	// try to find one
		if(aTag){
			this.removeNode(aTag, false)
			if(this.toolbar)
				this.toolbar.update();
		}
	}

	editor.prototype.cleanHTML = function(){
		this.html(getCleanHTML(this.html(), this.toolbar))
	}
	
	editor.prototype.isInTable = function(){
		return this.getNearestTagFromSelection("TABLE") ? true : false;
	}
	
	editor.prototype.setURLInfo = function(tag, info){
		setURLInfo(tag, info);
	}	

	editor.prototype.getURLInfo = function(tag){
		return getURLInfo(tag)
	}

	function setURLInfo(tag, info){
		
		tag.setAttribute("data-wga-urlinfo", info.type+"|"+info.key);

		var c = tag.className;
		c = c.replace(/wga-urltype-\w+ */, "");
		tag.className = c + (c ? " " : "") + "wga-urltype-" + info.type;

	}

	function getURLInfo(tag){
		var url = WGA.util.decodeURI(tag.href||tag.src||"");
		var parts = url.split("://");	
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

		var info = {
			path: path,
			domain: domain,
			protocol: protocol,
			type: "exturl",			// default
			key: url
		}
		
		var urlinfo = tag.getAttribute("data-wga-urlinfo") || tag.getAttribute("wga:urlinfo");
		if(urlinfo && domain==document.domain) {
			var parts = urlinfo.split("|");
			info.type=parts[0];
			info.key=parts[1];
		}
		return info;
	}
	
	function getCleanHTML(htmltext, toolbar){
		
		var good_els = "#h1#h2#h3#h4#h5#h6#a#img#p#pre#br#ul#ol#li#blockquote#";
		var good_els_textblock = "#br#div#";
		var bad_els = "#head#script#style#";
	
		var source = document.createElement("div");
		source.innerHTML = htmltext;
		
		var dest = document.createElement("div");
		var children = source.childNodes;
		for(var i=0; i<children.length; i++){
			copyElement(children[i], dest, toolbar ? good_els : good_els_textblock);
		}
		return dest.innerHTML;
		
		function hasToolbarClass(classes, cls){
			if(!classes)
				return false;
			for(var i=0; i<classes.length; i++){
				var c = classes[i].split("|");
				if(c[1]==cls)
					return true;
			}
			return false;
		}
	
		function copyElement(source, dest, good_els){
	
			if(source.nodeType==source.TEXT_NODE){					// text node 
				var txt = source.nodeValue.replace(/\xA0/g, " ");	// removes &nbsp;
				//console.log("created textnode", txt);
				dest.appendChild(document.createTextNode(txt));
			}
			else if(source.nodeType==source.ELEMENT_NODE || source.nodeType==source.DOCUMENT_FRAGMENT_NODE){
	
				if(source.style && source.style.display=="none")
					return;						// don't copy hidden elements
				if(source.style && source.style.visibility=="hidden")
					return;						// don't copy hidden elements
				
				var tagname=source.nodeName.toLowerCase();
				
				/*
				 * filter good and bad tags
				 */
				if(bad_els.indexOf("#"+tagname+"#")>=0){
					//console.log("ignoring bag tag " + tagname);
					return;		// we don't want this tag and it's contents
				}
				if(good_els.indexOf("#"+tagname+"#")>=0){
					
					/* special handling for pasted images in Safari: */
					if(tagname=="img" && source.src.indexOf("webkit-fake-url://")==0)
						return;		// image is useless so ignore this image
					
					/* 
					 * special tags attribute handling 
					 */
					var attributes=[];
					switch(tagname){
						case "a":
							attributes = ["href", "title", "data-wga-urlinfo", "wga:urlinfo"]
							break;
						case "img":
							attributes = ["src", "title", "alt", "data-wga-urlinfo", "wga:urlinfo"];
							break;
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
	
					// create element
					dest = dest.appendChild(document.createElement(tagname));
					//console.log("created element", tagname)
	
					// copy attributes
					for(var i=0; i<attributes.length; i++){
						var attribute = attributes[i];
						var val = source.getAttribute(attribute);
						if(val)
							dest.setAttribute(attribute, val);
					}
					
					// css classes
					if(toolbar){
						var cn = source.className.split(" ");
						var dest_classes=[];
						for(var i=0; i<cn.length; i++){
							var cls = cn[i];
							if(!cls)
								continue;
							if((tagname=="a" && hasToolbarClass(toolbar.linkStyleList, cls))
								|| (tagname=="img" && hasToolbarClass(toolbar.imageStyleList, cls))
								|| ("p,h1,h2,h3,h4,h5,h6,pre".indexOf(tagname)>=0  && hasToolbarClass(toolbar.paragraphStyleList, cls))
							)
								dest_classes.push(cls); 
						}
						if(dest_classes.length)
							dest.className = dest_classes.join(" ");
					}
					
					// special handling of womodo URLs
					if(tagname=="a" || tagname=="img"){
						setURLInfo(dest, getURLInfo(source));
					}
					
				}
				// else ignore this tag but copy it's contents
				
				/*
				 * copy child nodes
				 */
				var children = source.childNodes;
				for(var i=0; i<children.length; i++){
					copyElement(children[i], dest, good_els);
				}
				
			}
		}
	}	
	
	return {
		edit: function(el){
			return new editor(el);
		},
		getURLInfo: getURLInfo,
		setURLInfo: setURLInfo
	}
	
})
