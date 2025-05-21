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

		this.doc.execCommand("styleWithCSS", false, true);
		this.doc.execCommand("defaultParagraphSeparator", false, "p");
		
		this.el.on({
			"blur": function(ev){
				if(editor.toolbar && editor.toolbar.editorLostFocus)
					editor.toolbar.editorLostFocus(ev)
			},
			"keydown": function(ev){
				//console.log("keypress", ev.key, ev.which);
				$("img", el).removeClass("womodo-img-selected")
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
				else if(ev.key=="Enter"){
					var para = editor.getParagraph();
					if(ev.shiftKey || (para && para.tagName=="PRE")){
						editor.insertHTML("<br>")
						ev.preventDefault();
					}
					/*
					else if(!para){
						if(editor.textblock)
							editor.insertHTML("<br>");
						else editor.execCmd("formatBlock", "p")
					}
					*/
				}
			},
			"keyup focus click": function(ev){
				if(editor.toolbar){
					setTimeout(function(){
						editor.toolbar.update(editor)
					}, 100);
				}
			},
			"mousedown click": function(ev){
				// Safari/chrome/edge/FF64 don't select images
				$("img", el).removeClass("womodo-img-selected")
				if(ev.target && ev.target.tagName=="IMG"){			
					var imgel = ev.target;
					var sel = editor.getSelection();
					var range = editor.doc.createRange()
					range.setStartBefore(imgel);
					range.setEndAfter(imgel);
					sel.removeAllRanges()
					sel.addRange(range);
					
					$(imgel).addClass("womodo-img-selected")
					
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
			if(arguments.length)
				this.el.html(html)
			else {
				$("img", this.el).removeClass("womodo-img-selected")
				return this.el.html()
			}
		},
		close: function(){
			this.el.off()
			this.el.removeAttr("contenteditable");
		},
		getRange: function(){
			var sel = this.doc.getSelection()
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
			if(state=="sup"){
				return this.getNearestTagFromSelection("sup")
			}
			else if(state=="sub"){
				return this.getNearestTagFromSelection("sub")
			}
			else return this.doc.queryCommandState(state)
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
		var el=this.doc.createElement("span");
		el.innerHTML=html;

		//insert node at selection:
		range.deleteContents();	// deletes the selected Text
		range.insertNode(el);
		
		// Set Cursor after inserted HTML:
		var nextSibling = el.nextSibling;		// set cursor to this position later
		
		/*
		 * We have to clean up the DOM to avoid nested block-nodes:
		 * This is mainly an "intelligent copy" of DOM nodes.
		 * We create a new paragraph node and copy everything to it
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
			
			// copy styles and classes
			if(p.getAttribute("style"))
				copyTo.setAttribute("style",  p.getAttribute("style"));
			if(p.className)
				copyTo.className = p.className;
			
			p.parentNode.insertBefore(copyTo, p);

			copyAndSplitNode(p)

			p.parentNode.removeChild(p);
		}
		else this.removeNode(el)

		// set cursor after inserted HTML
		if(nextSibling){
			range.selectNodeContents(nextSibling);
			range.collapse(true);
			this.setRange(range);
		}
		
		function copyAndSplitNode(node){

			var blocktags = "p,h1,h2,h3,h4,h5,h6";
			var children = node.childNodes;
			for(var i=0; i<children.length; i++){
				var node = children[i];
				if(node==el){
					// this is the pasted <span> Tag. Move it childnodes to "newp"
					var haveBlockTag = false;
					var pastedNodes = el.childNodes;
					for(var j=0; j<pastedNodes.length; j++){
						var node = pastedNodes[j];
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
	
	editor.prototype.execCmd = function(cmd, param){
		
		if(cmd=="sup"){
			var node = this.getNearestTagFromSelection("sup");
			if(node)
				this.removeNode(node);
			else{
				var txt = this.getSelection()
				this.insertHTML("<sup>"+txt+"</sup>")
			}
		}		
		else if(cmd=="sub"){
			var node = this.getNearestTagFromSelection("sub");
			if(node)
				this.removeNode(node);
			else{
				var txt = this.getSelection()
				this.insertHTML("<sub>"+txt+"</sub>")
			}
		}		
		else this.doc.execCommand(cmd, false, param);
		
		if(cmd.toLowerCase()=="insertorderedlist" || cmd.toLowerCase()=="insertunorderedlist"){
			// Safari/Chrome bug: remove <span> element created by the browser.
			var span = this.getNearestTagFromSelection("span");
			if(span)
				this.removeNode(span);
		}
		
		if(this.toolbar)
			this.toolbar.update(this);
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
			return startNode.parentNode;		// Text node or other node types ...
		}

	}
	
	editor.prototype.getNearestTagFromSelection = function(tagNameToFind){
		tagNameToFind = tagNameToFind.toUpperCase();
		var range = this.getRange();
		var startNode=range.startContainer;
		if (startNode.nodeType==startNode.ELEMENT_NODE)		// Element
			return getNearestTag(this, startNode.childNodes[range.startOffset]||startNode, tagNameToFind);
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
			imgTag.alt = decodeURI(url.split("/").pop().split("?")[0]);
			var range = this.getRange();
			range.deleteContents();	// deletes the selected Text
			range.insertNode(imgTag);
		}
		imgTag.src=url;
		
		return imgTag;
	}

	editor.prototype.createLink=function(url, linktext){
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
	
	editor.prototype.deleteElement = function(el){
		var parent = el.parentNode;
		parent.removeChild(el);
	}
	
	editor.prototype.insertTableCol = function(){
		var td = this.getNearestTagFromSelection("TD");
		if(!td)
			return;
		var index = td.cellIndex+1;
		var rows=td.parentNode.parentNode.rows;
		for (i=0; i<rows.length; i++){
			tr=rows[i]
			if (tr.cells.length>=index)
				var new_td=tr.insertCell(index);	
			else
				var new_td=tr.insertCell();
			new_td.innerHTML="<br>";
		}
	}
	editor.prototype.deleteTableCol = function(){
		var td = this.getNearestTagFromSelection("TD");
		if (!td){
			return;
		}
		var table = this.getNearestTagFromSelection("TABLE")
		var index= td.cellIndex;
		var rows=td.parentNode.parentNode.rows;
		for (i=0; i<rows.length; i++){
			tr=rows[i];
			tr.deleteCell(index);
			if (table && tr.cells.length==0){
				this.deleteElement(table)	// last col: delete the table.
				break;
			}
		}
	}
	
	editor.prototype.insertTableRow = function(){
		var tr = this.getNearestTagFromSelection("TR")		
		if (!tr) {
			return;
		}
		var otr = tr.cloneNode(true);
		//clear row
		var tds = otr.getElementsByTagName("td");
		for (var i in tds) {
			var td = tds[i];
			try{
				td.innerHTML = "<br>";
			}
			catch(e){
				// may happen, if td contains a sub-table. Ignore this.
			}
		}
		tr.parentNode.insertBefore(otr, tr.nextSibling);
	}

	editor.prototype.deleteTableRow = function(){
		var tr = this.getNearestTagFromSelection("TR")
		var table = this.getNearestTagFromSelection("TABLE")
		tr && this.deleteElement(tr);
		if(table && table.rows.length==0)
			this.deleteElement(table)	// last row: delete the table.
	}
	
	editor.prototype.removeLink = function(aTag){
		if(!aTag)
			aTag=this.getNearestTagFromSelection("A");	// try to find one
		if(aTag){
			this.removeNode(aTag, false)
			if(this.toolbar)
				this.toolbar.update(this);
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
		
		/*
		 * The Link may be created via paste from another OpenWGA website.
		 * In this case we treat the link as "exturl" although linkinfo says something else.
		 * We check this by comparing the domain part of the url with the currently used domain. 
		 */
		var urlinfo = tag.getAttribute("data-wga-urlinfo") || tag.getAttribute("wga:urlinfo");
		if(urlinfo){
			if(domain==document.domain || urlinfo.indexOf("mailto|")==0){
				var parts = urlinfo.split("|");
				info.type=parts[0];
				info.key=parts[1];
			}
		}
		return info;
	}

	function getCleanHTML(htmltext, toolbar){
		
		var good_els = "#h1#h2#h3#h4#h5#h6#a#img#p#pre#br#ul#ol#li#table#tr#td#";
		var good_els_textblock = "#br#div#";
		var bad_els = "#head#title#meta#link#script#style#";
	
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
				var parts = classes[i].split("|");
				var c = parts.length>1 ? parts[1] : parts[0]; 
				if(c==cls)
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
							attributes = ["href", "title", "data-wga-urlinfo", "wga:urlinfo", "data-target", "target"]
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
								|| (tagname=="ul" && hasToolbarClass(toolbar.listStyleList, cls))
								|| (tagname=="table" && hasToolbarClass(toolbar.tableStyleList, cls))
								|| ("p,h1,h2,h3,h4,h5,h6,pre".indexOf(tagname)>=0  && hasToolbarClass(toolbar.paragraphStyleList, cls))
							)
								dest_classes.push(cls); 
						}
						if(dest_classes.length)
							dest.className = dest_classes.join(" ");
					}
					
					// special handling of womodo URLs
					if(tagname=="a" || tagname=="img"){
						var urlInfo = getURLInfo(source)
						setURLInfo(dest, urlInfo);
						if(tagname=="a"){
							// handle target attribute							
							var data_target = dest.getAttribute("data-target");
							if(!data_target){
								dest.setAttribute("data-target", "default");
								if(urlInfo.type=="exturl"){
									if(urlInfo.key.indexOf("mailto:")==-1 && urlInfo.key.indexOf("tel:")==-1)
										dest.setAttribute("target", "_blank");
									else dest.removeAttribute("target");
								}
							}
						}
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
