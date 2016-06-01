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
/**
 * RTFToolbar
 */
AFW.RTFToolbar=function(editor)
{
	// definition of methods:
	
	this.show=function(mode){
		this.mode=mode;	
		if (mode=="floating"){
			// Mozilla knows position=fixed to keep the toolbar in place on scrolling.
			// IE needs a timer to reposition the toolbar
			if (ie)
				this.tbdiv.style.position="absolute";
			else this.tbdiv.style.position="fixed";
			this.tbdiv.style.top=0;
			this.tbdiv.style.left=0;
			tb.tbdiv.style.width=document.body.clientWidth;
			window.onresize=function(){
				tb.tbdiv.style.width=document.body.clientWidth;
			}					
			document.body.insertBefore(this.tbdiv, document.body.firstChild);
			this.minMaxButton=document.createElement("button");
			this.minMaxButton.innerHTML="&lt;&lt;";		
			this.minMaxButton.onclick=function(){
				if(tb.tbarea.style.display=="none"){
					tb.tbarea.style.display="inline";
					tb.tbdiv.style.width=document.body.clientWidth;
					tb.minMaxButton.innerHTML="&lt;&lt;";
				}
				else{
					tb.tbarea.style.display="none";
					tb.tbdiv.style.width="20px";
					tb.minMaxButton.innerHTML="&gt;&gt;";
				}
			}
			this.tbdiv.appendChild(this.minMaxButton);
			if(ie){
				// in case of Mozilla the css position="fixed" does this for me
				this.stay_on_top();
			}
		}
		else {
			editor.orgElement.parentNode.insertBefore(this.tbdiv, editor.spanElement);
			tb.tbarea.className="WGABI_rtftoolbar";
		}
		
		this.tbdiv.onmouseover=function(){
			tb.mo=true;
		}
		this.tbdiv.onmouseout=function(){
			tb.mo=false;
		}
		if(tb.manager)
			tb.manager.hideOtherToolbars(tb.editor.id);
		this.update();
	}
	
	this.stay_on_top=function(){
		var yOffset = document.body.scrollTop;
		if(tb.tbdiv.style.top != yOffset + "px" ){
			//alert(this.tbdiv.style.top);									
			tb.tbdiv.style.top = yOffset;
		}
		tb.stay_on_top_timer=window.setTimeout(tb.stay_on_top, 250);
	}
	
	this.editorLostFocus=function(){
		return;
	}

	this.editorGotFocus=function(){
		if(tb.manager)
			tb.manager.hideOtherToolbars(tb.editor.id);
	}
	this.hide=function(hideMe){
		if(hideMe){
			tb.hideDialog();
			if(editor.viewmode!="wysiwyg")
				tb.editor.changeViewMode("wysiwyg");
			tb.tbdiv.style.display="none";
			if(tb.stay_on_top_timer){
				window.clearTimeout(tb.stay_on_top_timer)
				tb.stay_on_top_timer=null;
			}
		}
		else {
			tb.tbdiv.style.display="block";
			if(tb.mode=="floating" && ie && !tb.stay_on_top_timer)
				tb.stay_on_top();
		}
	}
	
	this.showContextMenu=function(ev){
		if (this.contextMenu){
			this.contextMenu.style.display="inline";
		}
		else{
			// create menu
			this.contextMenu=addElement(document.body, "div"); 
			with(this.contextMenu){
				className="WGABI_contextmenu";
				style.position="absolute";
				style.border="solid";
				style.backgroundColor="beige";
				style.borderWidth="2px";
			}

			var tmp=addElement(this.contextMenu, "div");
			tmp.className="menuentry";
			tmp.onclick=function(){ 
				tb.hideContextMenu();
				alert("click");
			}
			addText(tmp, "My Menu");
		}

		var el=this.editor.editelement;
		var x=0;
		var y=0;
		while(el){
			x += el.offsetLeft;
			y += el.offsetTop;
			el=el.offsetParent;
		}
		with(this.contextMenu.style){
			if(ie){
				left=x+ev.offsetX;
				top=y+ev.offsetY;
			}
			else{
				left=x+ev.clientX;
				top=y+ev.clientY;
			}
		}

		stopEvent(ev);
		//return false;
	}
	
	this.hideContextMenu=function(){
		if(this.contextMenu)
			this.contextMenu.style.display="none";
	}
	
	this.update = function(ev_type){
		if(ev_type=="blur")
			return;
		/**
			In mozilla the editor.doc is initialized in a timeout.
			If Mozilla is not ready, wait some time:
		*/
		if (!ie && !tb.editor.doc)
			window.setTimeout(tb.update, 100);
		else{
			if (tb.editor.isInTable() && tb.table_toolbar){			
				tb.table_toolbar.style.display="inline";
			}
			else if(tb.table_toolbar)
				tb.table_toolbar.style.display="none";
			
			for (var cmd in tb.buttons)
				_updateButton(cmd);
			for (var cmd in tb.selects)
				_updateSelect(cmd);
			tb.hideDialog();
			tb.hideContextMenu();
		}
	}
		
	function _updateButton(cmd){
		//alert(cmd);
		var btn=tb.buttons[cmd];		
		if (btn){
			try{
				btn.className= editor.doc.queryCommandState(cmd) ? "btn_selected" : "";
			} catch(e){
				// some buttons doesn't support queryCommandState
			}
			if (btn.queryDisable)
				tb.disableButton(cmd, btn.queryDisable());
		}
	}

	function _updateSelect(cmd){
		//alert("update select: " + cmd);
		var el=tb.selects[cmd];
		if (el){
			try{
				var sel=editor.doc.queryCommandValue(cmd);
				if (typeof(sel)=="string")
					sel=sel.toLowerCase();
				var opts=el.options;
				var index=0;
				for (var i=1; i<opts.length; i++){
					if (opts[i]!="" && opts[i].value.toLowerCase()==sel){
						index=i;					
						break;
					}
				}
				el.selectedIndex=index;
			} catch(e){
				// some cmd-s doesn't support queryCommandValue
			}
		}
	}
				
	this.hideDialog=function(){
		if (tb.currentdialog){
			tb.currentdialog.style.display="none";
			if (ie)
				tb.currentdialog.detachEvent("onkeydown", dialogKeyHandler);
			else document.removeEventListener("keypress", dialogKeyHandler, true);			
		}
		else return false;	// do nothing here and aviod focus-event to fire.

		tb.currentdialog=null;
		if(ie){
			if(tb.range && editor.viewmode=="wysiwyg"){
				try{
					tb.range.select();
				} catch (e){
					//IE somtine throws an "unkown exception" here.
				}
				tb.range=null;
				tb.editor.focus();
			}
		}
		else {
			tb.editor.focus();
			tb.editor.setRange(tb.range);
		}
		return false;	// Mozilla needs this. It autosubmit forms otherwiese
	}

	function dialogKeyHandler(event){
		if(event.keyCode==27){
			// ESC
			stopEvent(event);			
			tb.hideDialog();
		}
		else if(event.keyCode==13){
			if(tb.currentdialog && tb.currentdialog.submit){
				tb.currentdialog.submit();
				stopEvent(event);
			}
		}		
	}
	
	function stopEvent(event){
		// stop "normal" eventhandling:
		if (ie){
			event.cancelBubble = true;
			event.returnValue = false;
		}
		else {
			event.preventDefault();
			event.stopPropagation();
		}
	}
	
	function slideIn(){
		if (slide_h<slide_stop){
			slide_h+=slide_step;
			if (tb.currentdialog){
				tb.currentdialog.style.clip="rect(0px, " + tb.currentdialog.clientWidth+1 + ", " + slide_h + ", 0)";
				window.setTimeout(slideIn, 10);
			}
		}
	}

	this.showDialog=function(el){
		if (tb.currentdialog)
			tb.currentdialog.style.display="none";
		tb.currentdialog=el;
		//tb.currentdialog.style.clip="rect(0, 0, 0, 0)";		
		tb.currentdialog.style.display="inline";
		tb.currentdialog.style.position="absolute";
		
		// call init() method after dialog is visible.
		// This gives the init()-method the chance to hide some areas before the hight is calculated.
		if(el && el.init)
			el.init();
		
		/*				
		 * The following code deos not work with
		 * <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
		 * so we removed it.
		 
		slide_h=0;
		slide_stop=tb.currentdialog.clientHeight+10;
		slide_step=(slide_stop-slide_h)/30;
		//alert(slide_stop);
		window.setTimeout(slideIn, 100);
		*/
		
		if (ie){
			el.attachEvent("onkeydown", dialogKeyHandler);
		} else {
			document.addEventListener("keypress", dialogKeyHandler, true);			
		}		
	}
	this.resizeDialog=function(){
		tb.currentdialog.style.clip="rect(0px, " + tb.currentdialog.clientWidth+1 + ", " + tb.currentdialog.clientHeight+1 + ", 0)";
	}

	this.isCmdDisabled=function(cmd){
		// Helperfunction: check show/hide options.
		// showoptions count more: If command is in showoptions than show the button.
		// If not in showoptions but (and) in hideoptions: hide the button. Otherwise show it.
		if (this.showoptions.toLowerCase().indexOf(cmd.toLowerCase()) == -1 && 
				(
					this.hideoptions.toLowerCase().indexOf(cmd.toLowerCase()) != -1
					||
					this.hideoptions.toLowerCase().indexOf("all") != -1
				)){
			//alert("disabled: " + cmd);
			return true;
		}
		else {
			//alert("enabled: " + cmd);
			return false;
		}
	}
	
	this.addButton=function(parent, img, cmd, dialog_el, key){

		if (this.isCmdDisabled(cmd))
			return null;
			
		var img_el=document.createElement("img");
		img_el.src=WGA.contextpath+"/plugin-wga-app-framework/file/icons/"+img;
		
		var btn_el=document.createElement("button");
		btn_el.unselectable="On";
		
		var tt=AFW.RTFToolbar.txt.tooltips[cmd.toLowerCase()];
		if (tt!=undefined)
			img_el.alt=img_el.title=btn_el.title=tt;
		else img_el.alt=img_el.title=btn_el.title=cmd;

		if (dialog_el==undefined || dialog_el==null){
			// default click handler
			btn_el.onclick=function(){
				//alert(tb.editor.execCmd);
				tb.hideDialog();
				tb.editor.execCmd(cmd);
				return false;	// Mozilla needs this. It autosubmit forms otherwiese
			}
		}
		else btn_el.onclick=function(){
			if (dialog_el==tb.currentdialog)
				tb.hideDialog();
			else{
				tb.range=editor.getRange();	// save cursor position and selection. Will be restored in hideDialog()
				tb.showDialog(dialog_el);	// this will also call the dialogs init() method.
			}
			return false;	// Mozilla needs this. It autosubmit forms otherwiese
		}
				
		btn_el.appendChild(img_el);
		parent.appendChild(btn_el);
		
		this.buttons[cmd]=btn_el;	// store button element in list for status change
		if (key!=undefined)
			this.keys[key]=btn_el;	// store key for keyboard events called from editor.
		return btn_el;
	}
		
	this.addOptionlist=function(parent, cmd, opts){
		/*
		if (this.showoptions.toLowerCase().indexOf(cmd.toLowerCase()) == -1 && this.hideoptions.toLowerCase().indexOf(cmd.toLowerCase()) != -1)
			return;
		*/
		if (this.isCmdDisabled(cmd))
			return null;
			
		sel_el=document.createElement("select");
		sel_el.onchange=function(){
			tb.editor.execCmd(cmd, this.options[this.selectedIndex].value);
			tb.update();
		}
		for (var i=0;i<opts.length; i++){
			var el=document.createElement("option");
			var v=opts[i].split("|");
			if (v.length>1)
				el.value=v[1];
			else el.value=v[0];
			el.appendChild(document.createTextNode(v[0]));
			sel_el.appendChild(el);
		}
		parent.appendChild(sel_el);
		this.selects[cmd]=sel_el;
	}
	
	this.disableButton=function(btn, flag){
		tb.buttons[btn].disabled=flag;
	}
	
	this.disableAllButtons=function(mode){
		for (var i in tb.buttons)
			tb.buttons[i].disabled=mode;
		for (var i in tb.selects)
			tb.selects[i].disabled=mode;
	}

	this.doKeyFunction=function(key){
		if(this.keys[key]){
			//alert(key);
			this.keys[key].onclick();
			return true;
		}
		return false;
	}				

	//------------------------
	// Helper function for creating dynamic DOM elements:
	function addElement(el, tag){
		return el.appendChild(el.ownerDocument.createElement(tag));
	}
	function addText(el, txt){
		return el.appendChild(el.ownerDocument.createTextNode(txt));
	}
	function addImgButton(el, img, func){
		var el=addElement(el, "button");
		if (func != undefined)
			el.onclick=func;
		addElement(el, "img").src = WGA.contextpath+"/plugin-wga-app-framework/file/icons/"+img;
		return el;		
	}
	function createSelectElement(parent, optionlist){
		var sel_el=addElement(parent, "select");
		sel_el.unselecablte="On";
		for (var i=0; i<optionlist.length; i++){
			var el=addElement(sel_el, "option");
			var opt=optionlist[i].split("|");
			if (opt.length>1)
				el.value=opt[1];
			else el.value=opt[0];
			addText(el, opt[0]);
		}
		return sel_el;
	}
	
	function Dialog(parent, title, sectionTitle, width){

		// methods
		this.addButton=function(cmd, img){
			return tb.addButton(this.buttonTd, cmd, img);
		}
		this.addImgButton=function(img, func){
			addImgButton(this.buttonTd, img, func);
		}
		this.addInput=function(label){
			var tmpTr=addElement(this.table, "tr");		
			var tmpTd=addElement(tmpTr, "td");		
			tmpTd.noWrap=true;
			addText(tmpTd, label);
			tmpTd=addElement(tmpTr, "td");
			return addElement(tmpTd, "input");
		}
		this.addSelection=function(label, list){
			var tmpTr=addElement(this.table, "tr");		
			var tmpTd=addElement(tmpTr, "td");
			addText(tmpTd, label);
			tmpTd=addElement(tmpTr, "td");
			return createSelectElement(tmpTd, list);
		}
		this.addElement=function(tag){
			var tmpTr=addElement(this.table, "tr");		
			var tmpTd=addElement(tmpTr, "td");
			tmpTd.colSpan=2;
			return addElement(tmpTd, tag);
		}
		this.addSection=function(title){
			//addElement(this.el, "br");
			var fs_el=addElement(this.el, "fieldset");
			if(title!=undefined && title!="")
				addText(addElement(fs_el, "legend"), title);
			var tmpTable=addElement(fs_el, "table");
			//tmpTable.border=2; 
			tmpTable.align="left";
			tmpTable.valign="top";
			tmpTable.cellPadding=tmpTable.cellSpacing=0;
			//tmpTable.width="100%";
			this.table=addElement(tmpTable, "tbody"); // IE needs this!	
		}

		//***************
		// Constructor
		//***************
		
		this.el=addElement(parent, "div");		
		this.el.style.display="none";
		this.el.unselectable="On";
		this.el.className="dialog";
		if(width)
			this.el.style.width=width;
		//this.el.style.border="solid blue 3px";
			
		// create dialog title
		var tmp=addElement(this.el, "div");
		// tmp.style.border="solid red 3px";
		tmp.className="dialogTitle"; 
		var tmpTitleDiv=addElement(tmp, "div");
		tmpTitleDiv.className="dialogTitle"; 
		if(ie)
			tmpTitleDiv.style.styleFloat="left";
		else
			tmpTitleDiv.style.cssFloat="left"; 		
		addText(tmpTitleDiv, title);
		
 		this.buttonTd=addElement(tmp, "div");
		this.buttonTd.className="dialogTitle";
		this.buttonTd.align="right";  
		//this.buttonTd.style.styleFloat="right";
		//--- end dialog title
		
		// create fieldset
		this.addSection(sectionTitle);
		//--- end fieldset
	}
	
	// initialise toolbar with all buttons, dialogs an optionlists.
	this.init=function(hideoptions, showoptions){	

		this.hideoptions="ForeColor, FontName, FontSize, editHTML";	
		if (hideoptions!=undefined)
			this.hideoptions+=hideoptions;		// store options and validate it in addButton()/addOptionslist()
		if (showoptions!=undefined)
			this.showoptions=showoptions;		// store options and validate it in addButton()/addOptionslist()
		else this.showoptions="";
		
		/* Toolbar layout:
		tbdiv (div)				root element
		|-tbarea (span)			hidable area 
		  |-table
		    |-row 1: 			buttons
		    |-row 2: 			dialogs
		      |-dialog 1
		      |-...
		      |-dialog n
		|-button 				for show/hide toolbar area
		*/
		
		// create toolbar element. This will be appended to a document element in show()
		this.tbdiv=document.createElement("div");
		this.tbdiv.className="WGABI_rtftoolbar";
		this.tbdiv.unselectable="On";
		
		this.tbarea=addElement(this.tbdiv, "span");	// "hidable" area in case of floating toolbar
		
		// toolbar is a 2-row table.
		// row 1 contains the toolbar buttons
		// row 2 contains dialogs.
		var table_el=this.tbarea=addElement(this.tbarea, "table");
		table_el.cellPadding="0";
		table_el.cellSpacing="0";
		table_el.align="left";
		table_el=addElement(table_el, "tbody");
		// First table row: toolbar buttons
		var tr_el=addElement(table_el, "tr");
		var td_el=addElement(tr_el, "td");
		// second table row: dialog area:
		tr_el=addElement(table_el, "tr");
		this.dialogarea=addElement(tr_el, "td");
		
		this.currentdialog=null;
		
		// add toolbar buttons and optionslists
		if (editor.textarea)
			this.addButton(td_el, "save.gif", "save", null, "s");	
		if (ie)
			this.addButton(td_el, "paste_plain.png", "Paste");
			// in Mozilla and Safari the "paste" command is disabled to prevent security exploits.

		this.addButton(td_el, "arrow_undo.png", "Undo");
		this.addButton(td_el, "arrow_redo.png", "Redo");
		
		addElement(td_el, "span").className="separator";	
		
		this.addButton(td_el, "text_bold.png", "Bold", null, "b");
		this.addButton(td_el, "text_italic.png", "Italic", null, "i");
		this.addButton(td_el, "text_underline.png", "Underline", null, "u");
		this.addButton(td_el, "autocolor.gif", "ForeColor", null, "f");
		this.addButton(td_el, "html_delete.png", "RemoveFormat", null, "r");
		
		addElement(td_el, "span").className="separator";
		this.addButton(td_el, "text_subscript.png", "Sub", null);
		this.addButton(td_el, "text_superscript.png", "Sup", null);
		addElement(td_el, "span").className="separator";
		
		this.addOptionlist(td_el, "FormatBlock",
			["",
			"Paragraph|p",
			"Heading 1|h1",
			"Heading 2|h2",
			"Heading 3|h3",
			"Heading 4|h4",
			"Heading 5|h5",
			"Heading 6|h6"]);
		this.addOptionlist(td_el, "FontName", 
			["Default Font|", "Arial", "Times", "Verdana", "Helvetica", 
			"Courier", "FixedSys"]);
		this.addOptionlist(td_el, "FontSize",
			["Default FontSize|3",
			"1 (8 pt)|1",
			"2 (10 pt)|2",
			"3 (12 pt)|3",
			"4 (14 pt)|4",
			"5 (18 pt)|5",
			"6 (24 pt)|6",
			"7 (36 pt)|7"]);
		
		this.addButton(td_el, "text_indent.png", "Indent");
		this.addButton(td_el, "text_indent_remove.png", "Outdent");
		
		addElement(td_el, "span").className="separator";	
		
		this.addButton(td_el, "text_align_left.png", "JustifyLeft");
		this.addButton(td_el, "text_align_center.png", "JustifyCenter");
		this.addButton(td_el, "text_align_right.png", "JustifyRight");
		
		addElement(td_el, "span").className="separator";
		
		this.addButton(td_el, "text_list_bullets.png", "InsertUnorderedList");
		this.addButton(td_el, "text_list_numbers.png", "InsertOrderedList");
		
		addElement(td_el, "span").className="separator";
		
		// ************************************
		// create table function
		// show dialolg to ask for rows/columns
		// ************************************		
		var d=new Dialog(this.dialogarea, AFW.RTFToolbar.txt.tableDialog.title);
		var rows_input_el=d.addInput(AFW.RTFToolbar.txt.tableDialog.rows);
		var cols_input_el=d.addInput(AFW.RTFToolbar.txt.tableDialog.cols);
		var width_input_el=d.addInput(AFW.RTFToolbar.txt.tableDialog.width);
		var align_input_el=d.addSelection(AFW.RTFToolbar.txt.tableDialog.align, AFW.RTFToolbar.txt.tableDialog.alignValues);
		rows_input_el.size=cols_input_el.size=1;

		d.el.submit=function(){
			tb.hideDialog();
			var el_table=editor.createTable(rows_input_el.value, cols_input_el.value,
						width_input_el.value,
						align_input_el.value);
			return false;	// Mozilla needs this. It autosubmits forms otherwise
		};	
		// init() ist called when dialog is shown
		d.el.init=function(){
			rows_input_el.value="2";
			cols_input_el.value="3";
			width_input_el.value="100%";
			align_input_el.value=""; 
			rows_input_el.select();
		}
		
		d.addImgButton("accept.png", d.el.submit);
		d.addImgButton("cancel.png", this.hideDialog);
		
		this.addButton(td_el, "table_add.png", "InsertTable", d.el, "t");
		
		// ************************************
		// Table Functions (add-row, etc)
		// insert section to be hidden/displayed depending on editor.inInTable()
		// ************************************
		
		this.table_toolbar=addElement(td_el, "span");
		this.table_toolbar.style.display="none";

		// ************************************		
		// Dialog for Table Properties
		// ************************************
		
		var d=new Dialog(this.dialogarea, AFW.RTFToolbar.txt.tablePropDialog.title, "", 500);
		var el=d.addElement("span");
		
		addText(el, AFW.RTFToolbar.txt.tablePropDialog.change);
	
		// create option list depending on defined class names
		var list=new Array();
		if (this.tableStyleList.length>0)
			list.push(AFW.RTFToolbar.txt.tablePropDialog.tableStyleList[0] + "|table");
		if (this.trStyleList.length>0)
			list.push(AFW.RTFToolbar.txt.tablePropDialog.tableStyleList[1] + "|tr");
		if (this.tdStyleList.length>0)
			list.push(AFW.RTFToolbar.txt.tablePropDialog.tableStyleList[2] + "|td");
		
		if (list.length>0){	
			var classType=createSelectElement(el, list);
			classType.onchange=function(){
				switch(classType.options[classType.selectedIndex].value){
					case "table":
						tableStyleElement.style.display="inline";
						trStyleElement.style.display="none";
						tdStyleElement.style.display="none";
		
						tableApplyToElement.style.display="inline";
						trApplyToElement.style.display="none";
						tdApplyToElement.style.display="none";
						break;
					case "tr":
						tableStyleElement.style.display="none";
						trStyleElement.style.display="inline";
						tdStyleElement.style.display="none";
		
						tableApplyToElement.style.display="none";
						trApplyToElement.style.display="inline";
						tdApplyToElement.style.display="none";
						break;
					case "td":
						tableStyleElement.style.display="none";
						trStyleElement.style.display="none";
						tdStyleElement.style.display="inline";
						
						tableApplyToElement.style.display="none";
						trApplyToElement.style.display="none";
						tdApplyToElement.style.display="inline";
						break;
				}
			}
			
			addText(el, AFW.RTFToolbar.txt.tablePropDialog.to);
			
			// create TABLE style list
			var tableStyleElement=createSelectElement(el, AFW.RTFToolbar.txt.tablePropDialog.none.concat(this.tableStyleList));
			tableStyleElement.display="none";
			// create TR style list
			var trStyleElement=createSelectElement(el, AFW.RTFToolbar.txt.tablePropDialog.none.concat(this.trStyleList));
			trStyleElement.display="none";
			// create TD style list
			var tdStyleElement=createSelectElement(el, AFW.RTFToolbar.txt.tablePropDialog.none.concat(this.tdStyleList));
			tdStyleElement.display="none";
		
			addText(el, AFW.RTFToolbar.txt.tablePropDialog.applyto);
			var tableApplyToElement=createSelectElement(el, AFW.RTFToolbar.txt.tablePropDialog.applyToTableList);
			tableApplyToElement.display="none";
			var trApplyToElement=createSelectElement(el, AFW.RTFToolbar.txt.tablePropDialog.applyToTrList);
			trApplyToElement.display="none";
			var tdApplyToElement=createSelectElement(el, AFW.RTFToolbar.txt.tablePropDialog.applyToTdList);
			tdApplyToElement.display="none";

			d.el.submit=function(){
				tb.hideDialog();
				var type=classType.options[classType.selectedIndex].value;
				switch(type){
					case "table":
						editor.setClassName("table", tableStyleElement.options[tableStyleElement.selectedIndex].value);
						break;
					case "tr":
						editor.setTrClassName(trStyleElement.options[trStyleElement.selectedIndex].value, trApplyToElement.options[trApplyToElement.selectedIndex].value);
						break;
					case "td":
						editor.setTdClassName(tdStyleElement.options[tdStyleElement.selectedIndex].value, tdApplyToElement.options[tdApplyToElement.selectedIndex].value);
						break;	
				}
				return false;	// Mozilla needs this. It autosubmit forms otherwiese
			};
			
			d.el.init=function(){
				classType.onchange();
			}

			d.addImgButton("accept.png", d.el.submit);
			d.addImgButton("cancel.png", this.hideDialog);
			
			this.addButton(this.table_toolbar, "table_gear.png", "TableProperties", d.el);
		}
		// ************************************ END Table properties
		
		this.addButton(this.table_toolbar, "table_row_insert.png", "InsertTableRow");
		this.addButton(this.table_toolbar, "table_row_delete.png", "DeleteTableRow");
		this.addButton(this.table_toolbar, "tabaddcol.gif", "InsertTableCol");
		this.addButton(this.table_toolbar, "tabdelcol.gif", "DeleteTableCol");
		var btn=this.addButton(this.table_toolbar, "tabmergecells.gif", "MergeTableCells");
		if(btn){
			btn.queryDisable=function(){
				return !editor.mayMergeTableCells();
			}
		}
		btn=this.addButton(this.table_toolbar, "tabsplitcells.gif", "SplitTableCell");
		if(btn){
			btn.queryDisable=function(){
				return !editor.maySplitTableCell();
			}
		}
		addElement(this.table_toolbar, "span").className="separator";
		
		// ************************************		
		// create/update link with dialog
		// ************************************
		
		var d=new Dialog(this.dialogarea, AFW.RTFToolbar.txt.linkDialog.title);
		this.insertLinkDialog=d.el;

		d.el.input_el=d.addInput(AFW.RTFToolbar.txt.linkDialog.url);
		d.el.input_el.size=80;
		d.el.input_el.onchange=function(){
			//alert("URL changed manualy");
			tb.insertLinkDialog.linktype_el.value="exturl";
			tb.insertLinkDialog.iframe_el.style.display="none";
		}
		d.el.title_el=d.addInput(AFW.RTFToolbar.txt.linkDialog.linktitle);
		d.el.title_el.size=80;
		d.el.target_el=d.addInput(AFW.RTFToolbar.txt.linkDialog.target);
		d.el.target_el.size=20;
	
		d.el.linktype_el=d.addSelection(AFW.RTFToolbar.txt.linkDialog.linktype, AFW.RTFToolbar.txt.linkDialog.linktypeValues); 
		if (this.isCmdDisabled("InsertLink"))
			d.el.linktype_el.disabled=true;
		
		d.el.linktype_el.onchange=function(){
			var sel=this.options[this.selectedIndex].value;
			if(sel=="exturl"){
				tb.insertLinkDialog.iframe_el.style.display="none";
				tb.insertLinkDialog.input_el.readOnly=false;
			}
			else{
				tb.insertLinkDialog.iframe_el.style.display="inline";
				tb.insertLinkDialog.input_el.readOnly=true;
				tb.insertLinkDialog.input_el.value="";
				tb.initLinkSelectIFrame(tb.insertLinkDialog.iframe_el, sel);
			}
			tb.resizeDialog(tb.insertLinkDialog);
		}

		window.system = this;	// SiteExplorer needs this.
		this.fileEntrySelector=function(filename){
			// call back from Siteexplorer iframe	
			tb.insertLinkDialog.input_el.value = "../../file/"+editor.contentkey+"/"+filename;
			tb.insertLinkDialog.linktype_el.value="intfile";
			tb.insertLinkDialog.wgakey=filename;
		}

		d.el.iframe_el=d.addElement("iframe");
		d.el.iframe_el.width="100%";
		d.el.iframe_el.height="300px";
		d.el.iframe_el.style.display="none";
		//d.el.iframe_el.src=WGA.contextpath + "/static/html/blank.htm";
		
		d.el.submit=function(){
			tb.hideDialog(); 
			if (tb.insertLinkDialog.input_el.value!=""){
				var aTag=editor.createLink(tb.insertLinkDialog.input_el.value, "", tb.insertLinkDialog.linktype_el.value);
				aTag.target=tb.insertLinkDialog.target_el.value;
				aTag.title=tb.insertLinkDialog.title_el.value;
				aTag.removeAttribute("wga:urlinfo");	// remove this new CM 1.4-Attribute bc. we can't handle it
			}
			else editor.removeLink();
			return false;	// Mozilla needs this. It autosubmit forms otherwiese
		};		
		d.el.init=function(){	
			var init_link;
			var a_el=editor.getNearestTagFromSelection("A");
			if (a_el){
				tb.insertLinkDialog.input_el.value=a_el.href;
				tb.insertLinkDialog.target_el.value=a_el.target;
				tb.insertLinkDialog.title_el.value=a_el.title;
				
				var linkinfo = WGA.util.getLinkInfo(a_el);
				tb.insertLinkDialog.linktype_el.value=linkinfo.type;

				// test if we could store this value bc. it's e select box that only accept some valid vlaues
				if(tb.insertLinkDialog.linktype_el.value!=linkinfo.type)
					tb.insertLinkDialog.linktype_el.value="exturl";		// unkown type: use exturl				

				tb.insertLinkDialog.wgakey=linkinfo.key.split(".")[0];
				if (tb.insertLinkDialog.linktype_el.value=="intfile"){
					tb.insertLinkDialog.input_el.readOnly=true;
					tb.insertLinkDialog.iframe_el.style.display="inline";
					if(tb.insertLinkDialog.wgakey && tb.insertLinkDialog.wgakey!=""){
						init_link=tb.insertLinkDialog.wgakey;
					}
					tb.initLinkSelectIFrame(tb.insertLinkDialog.iframe_el, "intfile", init_link);
				}
				else{
					tb.insertLinkDialog.iframe_el.style.display="none";
				}
			}
			else{
				tb.insertLinkDialog.input_el.value="http://";
				tb.insertLinkDialog.target_el.value="";
				tb.insertLinkDialog.linktype_el.value="exturl";
				tb.insertLinkDialog.iframe_el.style.display="none";
				tb.insertLinkDialog.input_el.readOnly=false;
			}
			//tb.initLinkSelectIFrame(tb.insertLinkDialog.iframe_el, init_link);
			tb.insertLinkDialog.input_el.select();
			tb.insertLinkDialog.input_el.focus();
		}

		d.addButton("link_delete.png", "Unlink");
		d.addImgButton("accept.png", d.el.submit);
		d.addImgButton("cancel.png", this.hideDialog);
		
		this.addButton(td_el, "link.png", "InsertSimpleLink", d.el, "l");
		
		// ************************************		
		// Insert Image with dialog
		// ************************************
				
		var d=new Dialog(this.dialogarea, AFW.RTFToolbar.txt.imgDialog.title, "", 600);
		this.insertImgDialog=d.el;

		d.el.input_el=d.addInput(AFW.RTFToolbar.txt.imgDialog.url);
		d.el.input_el.size=80;
		d.el.alt_el=d.addInput(AFW.RTFToolbar.txt.imgDialog.alt);
		d.el.alt_el.size=80;
		d.el.border_el=d.addInput(AFW.RTFToolbar.txt.imgDialog.border);		
		d.el.align_el=d.addSelection(AFW.RTFToolbar.txt.imgDialog.align, AFW.RTFToolbar.txt.imgDialog.alignValues);

		d.el.iframe_el=d.addElement("iframe");
		d.el.iframe_el.allowTransparency=true; 		// IE needs this to show iframe transparent
		d.el.iframe_el.width="100%";
		//d.el.iframe_el.height="108px";
		d.el.iframe_el.height="200px";
		//d.el.iframe_el.style.border="none 0px";
		//d.el.iframe_el.frameBorder="0";
		d.el.iframe_el.src=WGA.contextpath + "/static/html/blank.htm";
		
		d.el.submit=function(){
			tb.hideDialog();
			var img=editor.createImg(tb.insertImgDialog.input_el.value, "intfile");
			img.border=tb.insertImgDialog.border_el.value;
			img.alt=img.title=tb.insertImgDialog.alt_el.value;
			img.align=tb.insertImgDialog.align_el.options[tb.insertImgDialog.align_el.selectedIndex].value;
			img.removeAttribute("wga:urlinfo");	// remove this new CM 1.4-Attribute bc. we can't handle it
			return false;	// Mozilla needs this. It autosubmit forms otherwiese
		};		
		d.el.init=function(){	
			window.dialogCallback=function(dblclick, url){
				tb.insertImgDialog.input_el.value=url;
				if(dblclick)
					tb.insertImgDialog.submit();
			}
			var iframesrc=WGA.contextpath + "/plugin-wga-app-framework/util:selectImage?dbkey=" + editor.dbkey + "&contentkey=" + editor.contentkey;
			if(tb.imageScaler!="")
				iframesrc+="?" + tb.imageScaler;
			tb.insertImgDialog.iframe_el.src=iframesrc;
			
			var img_el=editor.getNearestTagFromSelection("IMG");
			if (img_el){
				tb.insertImgDialog.input_el.value=img_el.src;
				tb.insertImgDialog.border_el.value=img_el.border;
				tb.insertImgDialog.alt_el.value=img_el.alt;
				tb.insertImgDialog.align_el.selectedIndex=0;
				if (img_el.align)
					for (var i=0; i<tb.insertImgDialog.align_el.options.length; i++)
						if(tb.insertImgDialog.align_el.options[i].value==img_el.align)
							tb.insertImgDialog.align_el.selectedIndex=i;
			}
			else{
				tb.insertImgDialog.input_el.value="http://";
				tb.insertImgDialog.border_el.value="0";
				tb.insertImgDialog.alt_el.value=""
				tb.insertImgDialog.align_el.selectedIndex=0;
			}			
			tb.insertImgDialog.input_el.select();
			tb.insertImgDialog.input_el.focus();
		}
		d.addImgButton("accept.png", d.el.submit);
		d.addImgButton("cancel.png", this.hideDialog);

		this.addButton(td_el, "picture.png", "InsertSimpleImg", d.el, "g");
				
		// -------------------------------------------------------
		
		// ************************************		
		// Fileupload dialog
		// ************************************
				
		var d=new Dialog(this.dialogarea, AFW.RTFToolbar.txt.fileDialog.title, AFW.RTFToolbar.txt.fileDialog.files, 400);
		//this.insertImgDialog=d.el;
		//d.el.dummy_el=d.addElement("div");
		//d.el.dummy_el.width="400";
		
		d.el.iframe_el=d.addElement("iframe");
		d.el.iframe_el.allowTransparency=true; 		// IE needs this to show iframe transparent
		d.el.iframe_el.width="385";
		d.el.iframe_el.height="162";
		d.el.iframe_el.style.border="none 0px";
		d.el.iframe_el.frameBorder="0";
		d.el.iframe_el.src=WGA.contextpath + "/static/html/blank.htm";

		d.el.init=function(){
			d.el.iframe_el.src=WGA.contextpath + "/plugin-wga-app-framework/util:selectfile?dbkey=" + editor.dbkey + "&contentkey=" + editor.contentkey;
			//d.el.iframe_el.src=WGA.contextpath + "/statictml/" + editor.dbkey + "/showFiles/" + editor.contentkey;
		}
		d.addImgButton("cancel.png", this.hideDialog);

		this.addButton(td_el, "attach.png", "UploadFile", d.el);
				
		// -------------------------------------------------------
		
		// -------------------------------------------------------
		
			
		addElement(td_el, "span").className="separator";
		
		// ************************************		
		// selection for edit/view-modes:
		// ************************************
		
		var viewmodes=AFW.RTFToolbar.txt.viewmodes;			// default: no HTML-editing
		if (this.showoptions.toLowerCase().indexOf("edithtml") != -1)		// allowed in showoptions.
			viewmodes=AFW.RTFToolbar.txt.viewmodes_html;
		sel_el=createSelectElement(td_el, viewmodes);		
		sel_el.unselecablte="On";
		sel_el.onchange=function(){
			if (ie && editor.viewmode=="wysiwyg")
				tb.range=editor.getRange();		// save cursor postion
			editor.changeViewMode(this.options[this.selectedIndex].value);
			//alert("changed to " + editor.viewmode);
			tb.hideDialog();
		}
	} // end function init();	


	//-----------------------------
	// constructor
	var ie=document.all;		// probably there is a better solution?

	var tb=this;
	this.editor=editor;
	editor.toolbar=this;		// inform the RTF-editor about the toolbar. This is used for toolbar-updates.
	
	// store Toolbar buttons and selects (created in addButton() and addOptionlist() for status change
	this.buttons=new Object();
	this.selects=new Object();
	this.keys=new Object();			// for keyboad events from editor.
	this.imageScaler="";

	// Styles (classes) for Table properties:
	this.tableStyleList="";
	this.trStyleList="";
	this.tdStyleList="";
	
};

// defined as prototype to that CM is able to overwrite this function.
AFW.RTFToolbar.prototype.initLinkSelectIFrame = function(iframe, type, key){
	var iframe = iframe;
	
	if(type=="int"){
		/*
		if(key){
			iframe.onload=function(){
				iframe.contentWindow.outline.selectEntry(key, false);
			};
		}
		iframe.src = WGA.contextpath + "/statictml/" + this.editor.dbkey + "/outlineStruct?menuFlag=0&language=en&asStructSelect=1";
		iframe.height="300px";
		*/
	}
	else if(type=="intfile" || type=="attachment"){
		
		//var url = WGA.contextpath + "/statictml/" + this.editor.dbkey + "/showFiles/" + this.editor.contentkey;
		var url = WGA.contextpath + "/plugin-wga-app-framework/util:selectfile?dbkey=" + this.editor.dbkey + "&contentkey=" + this.editor.contentkey;
		if(key)
			url += "?key="+key;
		iframe.src = url;
		iframe.height="162px";
	}
	else alert("initLinkSelectIFrame: unknown link type " + type);
}


AFW.RTFToolbarManager=function(){
	this.toolbars={};
	
	this.createToolbar=function(editor){
		var tb=new AFW.RTFToolbar(editor);
		tb.manager=this;
		this.toolbars[tb.editor.id]=tb;
		return tb;
	}
	this.hideOtherToolbars=function(id){
		for(var tb in this.toolbars){
			if (this.toolbars[tb].hide)
				this.toolbars[tb].hide(tb!=id);
		}
	}
	this.hideAllToolbars=function(){
		for(var tb in this.toolbars){
			if (this.toolbars[tb].hide)
				this.toolbars[tb].hide(true);
		}
	}
	this.getToolbar=function(id){
		return this.toolbars[id];
	}
	this.closeEditor=function(id){
		if (this.toolbars[id] && this.toolbars[id].hide){
			this.toolbars[id].hide(true);
			//this.toolbars[id].editor.changeViewMode("preview");
			this.toolbars[id].editor.closeEditor();
			//console.log("tb " + id + " closed");
		}
		else alert("Unable to close Toolbar. No Toolbar found for id " + id);
	}
	this.closeAllEditors=function(){
		for(var tb in this.toolbars)
			this.closeEditor(tb);
	}
};
