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
if(typeof(BI)=="undefined")
	BI={};

BI.util={
	emptyFN: function(){}
}

BI.util.guid = function() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return s4() + "_" + s4() + "_" + s4()
}

BI.util.getFileInfo=function(form, defaultName){

	if(typeof(form)=="string")
		form = document.forms[form];

	var f = defaultName;
	if(form.file)	
		f = form.file.value.replace(/\//g, "\\");		// replace / (used on unix systems) with \ (used on windows)
	
	var filepath=f.split("\\");
	var filename=filepath[filepath.length-1].split(".");
	var ext = filename[filename.length-1];
	if(form.scale && form.imgtype && form.scale.checked)
		ext = form.imgtype.value;
	filename.pop();		// remove file extension
	filename=filename.join(".");
	if(BI.sitepanel.iframe.window.WGA.translateFilename)
		filename=BI.sitepanel.iframe.window.WGA.translateFilename(filename)
	return {
		name: filename,
		ext: ext,
		filename: filename + "." + ext
	}
}


BI.util.anim=new function(){
	this.animating={};
	
	this.slideIn=function(el, callback){
		var el=Ext.get(el);
		if(this.animating[el.dom])
			return;
		this.animating[el.dom]={cb:callback};
		el.slideIn("t", {
			scope: this,
			callback: function(){
				if(this.animating[el.dom].cb)
					this.animating[el.dom].cb();
				delete this.animating[el.dom];
			}
		})			
	};
		
	this.slideOut=function(el, callback){
		var el=Ext.get(el);
		if(this.animating[el.dom])
			return;
		this.animating[el.dom]={cb:callback};
		el.slideOut("t", {
			useDisplay: true,
			scope: this,
			callback: function(){
				if(this.animating[el.dom].cb)
					this.animating[el.dom].cb();
				delete this.animating[el.dom];
			}
		})			
	}
	
}();

BI.util.expandElement=function(el, callback){
	Ext.get(el).slideIn("t", {
		scope: el,
		callback: callback
	})
}

BI.util.collapseElement=function(el, callback){	
	Ext.get(el).slideOut("t", {
		scope: el,
		useDisplay: true,
		callback: callback
	})
}

BI.util.toggleElement=function(el, callback){
/*	if(el.style.display=="none")
		BI.util.expandElement(el, callback)
	else BI.util.collapseElement(el, callback)
	*/
	if(el.style.display=="none")
		BI.util.anim.slideIn(el, callback)
	else BI.util.anim.slideOut(el, callback)
	
}

/**
 * calls a WGA-Action using AJAX and showing message while loading
 * @param {Object} action
 * @param {Object} id
 * @param {Object} message
 */
BI.util.callAction=function(action, id, message){

	WGA.ajax.b4post.register(id, function(){
		var el = Ext.get("$ajaxContentDiv_" + id);
		el.update("<b>"+message+"</b><hr size='1'><img src='"+BI.wgapath+"/file/images/loading.gif'/>&nbsp;" + $L.please_wait + " ...")
	})

	WGA.ajax.action({
		action: action,
		id: id,
		graydiv: false
	});
}

/**
 *  Utility function to choose/delete virtual link
 */
BI.struct=function(){

	var form;
	
	return{	

		chooseVLink: function(el){

			var form=el.form;
			var linktype = form.virtuallinktype.value||"exturl";
			var path = decodeURI(form.virtuallink.value).split("/");
			var wgakey;
			if(linktype=="file" || linktype=="extfile"){
				var filename=path.pop();
				var container=path.pop();
				wgakey=container+"/"+filename;
			}
			else if (linktype=="exturl")
				wgakey=form.virtuallink.value;
			else wgakey=path.pop();		// last element in path						

			var opts={
				vlink: true,
				linktype: linktype,
				wgakey: wgakey,
				target: form.linktarget.value 
			};
			BI.dialog.show("rtf:insert-link", null, opts);
			BI.dialog.callback=function(url, linktext, linkinfo, target){
				linktype=linkinfo.type;
				form.virtuallink.value=linkinfo.key;
				form.virtuallinktype.value=linktype;
				form.virtuallinktitle.value=linktext;
				form.linktarget.value=target||"";
				document.getElementById("virtuallinkinfo").innerHTML=$L.linktype[form.virtuallinktype.value] + " - " + form.virtuallink.value+" (<b>"+linktext+"</b>)<br>";
			}
		},
		
		deleteVLink: function(el){
			var form=el.form;
			form.virtuallink.value="";
			form.virtuallinktype.value="";
			form.linktarget.value="";
			form.virtuallinktitle.value="";
			document.getElementById("virtuallinkinfo").innerHTML="";
		}
		
	}
}();

/**
 * convert all <input class="datefield"> to Ext.date-fields
 * @param {Object} element: element to search below in the dom tree for <input>-s
 * @param {Object} format: format of the date (in Ext format)
 */
BI.makeDateFields=function(element, format){
	var dh = Ext.DomHelper;
	var els=Ext.get(Ext.query("input.datefield", element));
	els.each(function(el){
		var span = dh.insertBefore(el, {tag:"span"})
		var d = new Ext.form.DateField({
			el: el.dom,
			format: format || "d.m.Y"
		});
		d.render(span);
		//d.applyTo(el);
		//console.log("datefield: ", format, el.dom.name)
	})
}

/**
 * make all textareas resizable
 * @param {Object} element: element to search below in the dom tree for <textarea>-s
 */
BI.makeTextareasResizable=function(element){
	var dh = Ext.DomHelper;
	var els=Ext.get(element).select("textarea");
	//console.log(els);
	els.each(function(el){
		var span = dh.insertBefore(el, {tag:"span"})
		var d = new Ext.form.TextArea({
			el: el.dom,
			minHeight: 24,
			grow: true
		});
		d.render(span);
		d.autoSize();
	})
}

/**
 * convert <select>-s to Ext.Combobox
 * @param {Object} element: element to search below in the dom tree for <select>-s
 */
BI.makeComboBoxes=function(element){
	var dh = Ext.DomHelper;
	var els=Ext.get(Ext.query("select", element));
	els.each(function(el){
		var onchange=el.dom.onchange;
		d = new Ext.form.ComboBox({
			typeAhead: true,
			triggerAction: 'all',
			forceSelection:true,
			editable: false,
			listAlign: 'tl-bl?',
			width: 300,
			//autoCreate : {tag: "input", type: "text", size: "45", autocomplete: "off"},
			transform: el.dom
		});
		if(onchange)
			d.on("select", onchange);
	})
}

BI.makeInputFields=function(el){
	var els=Ext.get(Ext.query("input[@type='text'],input[@type='password']", el));
	els.addClass("x-form-text");
	//els.set({autocomplete: "off"});
}

/**
 * create an EXT.tree showing the structure of the site
 * @param {Object} el: the element to render the tree in
 * @param {Object} dbkey: dbkey of database 
 */
BI.util.structTree=function(el, dbkey, lang, loaderurl){

	var Tree = Ext.tree;
    
	this.tree = new Tree.TreePanel(el, {
        animate:true, 
        loader: new Tree.TreeLoader({
        	dataUrl:loaderurl || './cms:siteexplorer:get-struct-nodes.int.html',
        	baseParams: {
        		dbkey: dbkey||BI.dbkey,
				lang: lang||BI.language
        	}
        }),
        enableDD:false,
        rootVisible:false,
        singleExpand: true
    });

    var root = new Tree.AsyncTreeNode({
        text: dbkey,
        icon: "../file/database.png",
        cls: "root", 
        draggable:false, // disable root node dragging
        id:'root'
    });
    
    this.tree.setRootNode(root);
    
    this.tree.render();
    root.expand(false, true);
	
}

BI.util.dom={};

BI.util.dom.getXY=function(el){

	var x = el.offsetLeft;
	var y = el.offsetTop;
	var p = el.offsetParent;

	while (p && p.tagName.toLowerCase() != "body") {
		x += p.offsetLeft;
		y += p.offsetTop;
		p = p.offsetParent;
	}
	return [x,y]
}

BI.util.dom.setXY=function(el, xy){
		
	var dh = Ext.DomHelper;
	dh.applyStyles(el, {position:'absolute', left:xy[0]+"px", top:xy[1]+"px"})
	
	/*
	 * 	IE doesn't like this old way
	 */
	/*	
	el.style['position']="ablolute";
	el.style.left = xy[0] + "px";
	el.style.top = xy[1] + "px";
	*/
}

/**
 * get width of window (viewport)
 * code copied from YAHOO.util.Dom but use "window" as parameter
 * @param {Object} window
 */
BI.util.dom.getViewportWidth = function(window) {

	var document = window.document;
	var width = window.innerWidth;  // Safari
	try{
	    var mode = document.compatMode;
	
	    if (mode || WGA.isIE) { // IE, Gecko, Opera
	        width = (mode == 'CSS1Compat') ?
	                document.documentElement.clientWidth : // Standards
	                document.body.clientWidth; // Quirks
	    }
	}
	catch(e){
		// try-catch avoids JS-error when window has no document.
		// Don't know why this happens, but it happens.
	}
	
    return width;
}

BI.util.dom.getViewportHeight=function(window) {
	var document = window.document;
    var height = window.innerHeight; // Safari, Opera
    var mode = document.compatMode;

    if ( (mode || BI.isIE) && !BI.isOpera ) { // IE, Gecko
        height = (mode == 'CSS1Compat') ?
                document.documentElement.clientHeight : // Standards
                document.body.clientHeight; // Quirks
    }
    return height;
}

/**
 * Returns the height of the document.
 * code copied from YAHOO.util.Dom but use "window" as parameter
 * @method getDocumentHeight
 * @return {Int} The height of the actual document (which includes the body and its margin).
 */

BI.util.dom.getDocumentHeight= function(window) {
	var document = window.document;
    var scrollHeight = (document.compatMode != 'CSS1Compat') ? document.body.scrollHeight : document.documentElement.scrollHeight;
    var h = Math.max(scrollHeight, BI.util.dom.getViewportHeight(window));
    return h;
},

/**
 * Returns the width of the document.
 * code copied from YAHOO.util.Dom but use "window" as parameter
 * @method getDocumentWidth
 * @return {Int} The width of the actual document (which includes the body and its margin).
 */

BI.util.dom.getDocumentWidth= function(window) {
	var document = window.document;
    var scrollWidth = (document.compatMode != 'CSS1Compat') ? document.body.scrollWidth : document.documentElement.scrollWidth;
    var w = Math.max(scrollWidth, BI.util.dom.getViewportWidth(window));
    return w;
}

BI.util.renderFileSize = function(value) {
	// start with GB and work way down to kB
	var kb = 1000;
	var mb = kb*kb;
	var gb = mb*kb;				
	if (value >= gb)
		return '<span style="color:red">'+ Math.round(value/gb) + ' GB</span>';
	else if (value >= mb)
		return '<span style="color:red">'+ Math.round(value/mb) + ' MB</span>';
	else if (value >= kb)
		return Math.round(value/kb) + ' KB';
	else
		return value + ' bytes';		
}

BI.util.createAttachmentGrid=function(el, type, key, autoHeight, dragdrop, lazy_load, onload){
	
	var id=el;
	
	var ds = new Ext.data.Store({
        proxy: new Ext.data.HttpProxy(new Ext.data.Connection({
        		url: "./json:get-attachments.int.html"
        })),
        reader: new Ext.data.JsonReader(
        	{
        		id: "id",
        		totalProperty: "total",
        		root: "data"
        	},
         	[
         		{name: 'id'},
         		{name: 'filetype'},
               	{name: 'primary'},
               	{name: 'name'},
               	{name: 'url'},
               	{name: 'poster'},
               	{name: 'mimetype'},
               	{name: 'thumbnail'},
               	{name: 'size', type: 'float'}
          	]
     	)
    });
    if(onload)
    	ds.on("load", onload);
    	
    var urlparams={dbkey: BI.dbkey, type:type, key: key, who:el};
    if(!lazy_load)
    	ds.load({params:urlparams});
    
    var colModel = new Ext.grid.ColumnModel([
    	{
    		id: "col-primary",
    		header: "P",
    		width: 16,
    		dataIndex: 'primary',
    		renderer: function(value){
    			return value?"*":""
    		}
    	},
		{
			id: "col-filename", header: $L.filename, width: 40, sortable: true, dataIndex: 'name',
				editor: new Ext.grid.GridEditor(new Ext.form.TextField({
	            	allowBlank: false
	          	}))
		},
		{id: "col-size", header: $L.filesize, width: 50, resizable:false, align:"right", renderer: BI.util.renderFileSize, sortable: true, dataIndex: 'size'}
	]);
	
    // create the Grid
    var grid = new Ext.grid.EditorGrid(el, {
        ds: ds,
        cm: colModel,
        selModel: new Ext.grid.RowSelectionModel(),
        enableDragDrop: dragdrop||false,		// enable drag&drop. Defaults to false.
        autoHeight: autoHeight||false,
        autoExpandColumn: "col-filename"
    });
	grid.render();
	
	return grid;
}

BI.util.editDivText=function (el, value, callback, region){
	var dh = Ext.DomHelper;
	var span = dh.insertBefore(el, {tag:"span"})
	//var info = dh.insertBefore(el, {tag:"div", cls:"field-edit-message", html:"press ESC to cancel"});
	var f = new Ext.form.TextField({
		originalElement: el,
		callback: callback,
		region: region,
		restore: function(){
			span.parentNode.removeChild(span);
			//info.parentNode.removeChild(info);
		},
		width: Ext.get(el).getWidth(),
		qtip: "ESC to cancel edit",
		value: value	//el.innerHTML
	});
	el.style.display="none";
	f.render(span);
	if(region)
		region.pauseAutoHide(true);
	f.on("specialkey", function(field, ev){
		if(ev.getKey() == ev.ENTER){
			ev.preventDefault();
			field.el.dom.blur();
		}
		if(ev.getKey() == ev.ESC){
			field.reset();
			field.el.dom.blur();
		}
	})
	f.on("change", function(field){
		field.originalElement.innerHTML = field.getValue();
		field.callback(field.getValue())
	});
	f.on("blur", function(field){
		field.restore();
		field.originalElement.style.display="block";
		if(field.region)
			region.pauseAutoHide(false);
		field.destroy();
	});
	f.focus(true);
}

BI.util.elementUpdater={
	render: function(el, response, updateManager, callback){	
		// IE does not recognize the first script tag by getElementsByTagName
	    // if there is no html:tag with content in front
	    // so add a none displayed span-tag in front of the pasted code
		el.update("<span style=\"display:none\">ajax</span>" + response.responseText);
		
	    // evaluate pasted javascript
	    var scripts = el.dom.getElementsByTagName("script");
		var totalscripts="";
	    for (var i = 0; i < scripts.length; i++) {
			var script = scripts[i].innerHTML;
			totalscripts += "\n"+script;
		}
		window.setTimeout(totalscripts, 10);
		if(callback)
			callback();
		//console.log("BI.elementUpdater called", el)
	}
}

BI.util.encode=function(t){
	return t.replace(/</g, "&lt;").replace(/>/g, "&gt;")
}

BI.util.getLinks=function(itemnames, unencoded){

	var fieldtype = unencoded===false ? "":"_unencoded";
	var links={
		internal: [],
		external: [],
		count: 0
	}

	var baseurl = BI.dbpath+"/";
	var win=BI.sitepanel.iframe.window;
	if(!win)
		return links;
	var doc=win.document;

	var items;
	if(itemnames instanceof Array)
		items = itemnames;
	else items = itemnames.split(",");

	for(var i=0; i<items.length; i++){
		var item = items[i].toLowerCase();
		try{
			var els=Ext.get(Ext.query("#item_"+item+fieldtype+" img, #item_"+item+fieldtype+" a", doc.body));
			els.each(function(domel, els, index){
				var href= this.dom.href||this.dom.src;				
				if(!href)
					return;		// ignore anker links

				// issue #00002152: Link checker no longer available if an RTF contains images
				var href_attr = this.dom.getAttribute("href");
				if(href_attr && href_attr.indexOf("#")==0)
					return; 		// this is a local anker link

				var prot = href.split("://")[0];
				if(prot=="http" || prot=="https"){
				
					var info = AFW.RTF.getURLInfo(this.dom);
					var url;
					switch(info.type){
						case "int":
						case "intname":
							url = "html/default/" + info.key;
							break;
						case "intfile":
							url = "file/" + info.key;
							break;
						case "file":
						case "extfile":
							url = "file/"+info.key;
							break;
						default:	// must be exturl
							url = WGA.util.decodeURI(href.split("?")[0]);	// ignore URL parameter
					}
					if(info.type=="exturl"){
						links.external.push({
							el: this.dom,
							url: href
						});
					}
					else{
						links.internal.push({
							el: this.dom,
							item: item,
							url: url
						});	
					}
					links.count++;

				}
			})	
		}
		catch(e)
		{
			// ignore any exceptions caused by item names containing non valid CSS-selectors (#, &, .)
			// console.log(e);
		}			
	}
	return links;
}

BI.util.markLinks=function(links, url, msg){

	function markLink(el){
		el.title=msg;
		if(link.el.tagName=="IMG"){									
			el.src = BI.wgapath + "/file/images/file_broken.png";
		}
		else if (el.tagName=="A"){
			el.style.backgroundColor="brown";
			el.style.color="white";
			if(el.innerHTML=="")
				el.innerHTML="invisible link"
		}
	}

	for(var i=0; i<links.internal.length; i++){
		var link = links.internal[i];
		if(link.url==url){
			markLink(link.el);
		}								
	}
	for(var i=0; i<links.external.length; i++){
		var link = links.external[i];
		if(link.url==url){
			markLink(link.el);
		}								
	}
}


BI.util.trim=function(str) {
	return str.replace (/^\s+/, '').replace (/\s+$/, '');
}


BI.util.ImageScaler = function(image, thename, thesize) {
	var thumb = Ext.get(image);
	var img = thumb.dom;

	img.style.display = "block";
	img.title = thename + "\n(" + Math.round(thesize/1000) + " KB)"
	
	var wMax = 80;
	var hMax = 60;
	
	if (Ext.isIE) {
		// driffrent padding/margin behaviour in IE
		wMax -= 2 * 3;
		hMax -= 2 * 3;
	}
	
	// workaround for safari timing problem
	var ignoreMe = img.offsetWidth;
	ignoreMe = img.offsetHeight;
	
	if (img.width > wMax || img.height > hMax) {
		img.height = hMax;
		
		if(img.width > wMax){
			img.height = hMax * (wMax / img.width);
			img.width = wMax;
		}
	}
	
	// drifferent behaviours in diffrent browsers, thus some fine tuning via static value :-(
	var static = -8;
	if (Ext.isSafari) static = -7;
	if (Ext.isIE) static = -7;
	
	var wrapper = Ext.get(image.parentNode.parentNode);
	thumb.setY((wrapper.getY() + wrapper.getHeight()/2 + static) - img.height/2);
	thumb.setX((wrapper.getX() + wrapper.getWidth()/2) - img.width/2);
}


BI.util.attachmentViewDragZone = function(config){
	Ext.apply(this, config, {
		no_images_label: "No Images found",
		sitepanel_mask_msg: ""
	});
	BI.util.attachmentViewDragZone.superclass.constructor.call(this, this.view.getEl(), {});
}

Ext.extend(BI.util.attachmentViewDragZone, Ext.dd.DragZone, {

	getDragData :function(e){
		var target = e.getTarget('.thumb-wrap');
		var view = this.view;
		var store = this.view.store;
		
		if(target){
			
            if(!view.isSelected(target)){
                var id = target.id.split("/");
                id.pop();
                id = id.join("/");
                var record = store.getById(id);
                return {
                	grid: view,
                	selections: [{json:record.data}],
                	ddel: target,
                	single: true
                }
            }
            
            var selNodes = view.getSelectedNodes();
            
            var selections = [];
            for(var i=0; i<selNodes.length; i++){
            	var id = selNodes[i].id.split("/");
                id.pop();
                id = id.join("/");
            	var record = store.getById(id);
            	if(record){
            		selections.push({
            			json: record.data
            		})
            	}
            }
            
            var dragData = {
                selections: selections,
                grid: view
            };
            
            if(selNodes.length == 1){
                dragData.ddel = target;
                dragData.single = true;
            }else{
                var div = document.createElement('div'); // create the multi element drag "ghost"
                div.className = 'multi-proxy';
                div.innerHTML = i + ' ' + this.no_images_label;
                dragData.ddel = div;
                dragData.multi = true;
            }
        	return dragData;
		}
		else return false;
	},

	onStartDrag: function(){
		BI.sitepanel.mask(this.sitepanel_mask_msg);
		return true;
	},
	
	onEndDrag: function(data, e){
		BI.sitepanel.unmask();
		return true;
	}

});

BI.util.GroupedView = function(container, tpl, config){
	this.addEvents({
		"refresh": true
	});
	BI.util.GroupedView.superclass.constructor.call(this, container, tpl, config);
}
Ext.extend(BI.util.GroupedView, Ext.View, {

	refresh : function(){

        var t = this.tpl;
        this.clearSelections();
        this.el.update("");
        var html = [];
        var records = this.store.getRange();
        if(records.length < 1){
            this.el.update(this.emptyText);
            return;
        }

        var group, col=0;
        for(var i = 0, len = records.length; i < len; i++){
            var data = this.prepareData(records[i].data, i, records[i]);
            if(this.groupchange && data[this.groupchange]!=group){
            	group = data[this.groupchange]
            	html.push('<div class="groupchange">' + group + '</div>')
            	col=0; 
            }
            if(this.cols && col%this.cols==0)
            	data.clear="clear:left"
            html.push(t.apply(data));
            col++
        }
        this.el.update(html.join(""));
        this.nodes = this.el.dom.childNodes;
        this.updateIndexes(0);
        
		this.fireEvent("refresh", this.el);
	}
})
