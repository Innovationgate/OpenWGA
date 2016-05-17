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
BI.ComboTree = Ext.extend(Ext.form.TriggerField, {
   
    // private
    defaultAutoCreate : {tag: "input", type: "text", size: "24", autocomplete: "off"},
    listClass: '',
    selectedClass: 'x-combo-selected',
    triggerClass : 'x-form-arrow-trigger',
    shadow:false,
    listAlign: 'tl-bl?',
    maxHeight: 300,
    resizable: false,
    handleHeight : 8,
    minListWidth : 170,
    minListHeight : 200,
    lazyInit : false,
    
    loaderurl : null,
    
    valueField:  'id',
    
	sitemap : null,
	tree : null,
    
    initialRender: true,
	
    initComponent : function() {
    	
      	BI.ComboTree.superclass.initComponent.call(this);
		
		this.addEvents({
			'expand' : true,
			'collapse' : true,
			'beforeselect' : true,
			'select' : true
		});
    }, 


    // private
    onRender : function(ct, position){
                  
        BI.ComboTree.superclass.onRender.call(this, ct, position);
       
        this.hiddenField = Ext.get(this.inputField);
         
	    if(this.hiddenName){
	      this.hiddenField = Ext.get(this.inputField) || this.el.insertSibling({tag:'input', type:'hidden', name: this.hiddenName, id: (this.hiddenId||this.hiddenName)},
	              'before', true);
	      this.hiddenField.value =
	          this.hiddenValue !== undefined ? this.hiddenValue :
	          this.value !== undefined ? this.value : '';
		
	      this.el.dom.removeAttribute('name');
	    }
			
	    this.el.dom.setAttribute('readOnly', true);
	    this.el.on('mousedown', this.onTriggerClick,  this);
	    this.el.addClass('x-combo-noedit');
	    
	    // reposition trigger for safari
	    if (Ext.isSafari) {
	    	this.trigger.setX(this.el.getX() + this.width - this.trigger.getWidth());
	    }
		
	    if(!this.lazyInit){
	        this.initList();
	    }else{
	        this.on('focus', this.initList, this, {single: true});
   		}
    },
    

	createTree: function() {
		
		this.sitemap = new BI.util.structTree(this.innerList, this.dbkey||BI.dbkey, this.language||BI.language, this.loaderurl);
		this.tree = this.sitemap.tree;
		
		this.tree.on("click", function(node, e) {
	    	var a = node.id.split(":");
        	if(a[0]=="node-struct" && !node.disabled) {
        		this.setValue(a[1], node.attributes.text);
        		this.collapse();
        	}
        	this.onSelect(node, e)
	    }, this);
		
		this.tree.on("expand",   this.restrictListHeight, this);
		this.tree.on("collapse", this.restrictListHeight, this);		
	},



	selectPath : function(path) {
		var combo = this;
		this.tree.selectPath(path, null,function(success, node) {
			if (success) {
				combo.setValue(node.id.split(":")[0], node.attributes.text);	
				combo.path = null;
			}
		});
	},



    initList : function(){
  
		if(!this.list){
			var cls = 'x-combo-list';
			
			this.list = new Ext.Layer({
				shadow: this.shadow, cls: [cls, this.listClass].join(' '), constrain:true, zindex: 50000
			});
			
			this.innerList = this.list.createChild({cls:cls+'-inner'});
			
			
			this.assetHeight = 0;
			
			// render Tree!
			
			this.list.setWidth(this.width || this.minListWidth);
        	this.innerList.setWidth(this.width || this.minListWidth);	
   		}
   		
		this.createTree();
		
		if (this.path) this.selectPath(this.path);
		
    },


    // private
    initEvents : function(){
        BI.ComboTree.superclass.initEvents.call(this);
        
        // key ESC will collapse the combo
         this.keyNav = new Ext.KeyNav(this.el, {
           
            "esc" : function(e) {
                this.collapse();
            },
            
            scope : this,
            forceKeyDown: true
         });
         this.keyNav.disable();
    },

    onDestroy : function(){
       
        if(this.list){
            this.list.destroy();
        }
        
        if (this.tree) {
        	this.tree.destroy();
        }

        BI.ComboTree.superclass.onDestroy.call(this);
    },
	
	
    // private
    fireKey : function(e){
        if(e.isNavKeyPress() && !this.isExpanded() && !this.delayedCheck){
            this.fireEvent("specialkey", this, e);
        }
    },

  

    // private
    onEnable: function(){
        BI.ComboTree.superclass.onEnable.apply(this, arguments);
        if(this.hiddenField){
            this.hiddenField.disabled = false;
        }
    },

    // private
    onDisable: function(){
        BI.ComboTree.superclass.onDisable.apply(this, arguments);
        if(this.hiddenField){
            this.hiddenField.disabled = true;
        }
    },


    // private
    onSelect : function(node, e){       
        if (this.fireEvent("beforeselect", this, node, e) !== false) {
	        this.fireEvent("select", this, node, e);
        }
    },
    

   
    getValue : function(){
        if(this.valueField){
            return typeof this.value != 'undefined' ? this.value : '';
        }else{
            return BI.ComboTree.superclass.getValue.call(this);
        }
    },

 
	     
    setValue : function(v, t){
      	if (!v || !t)
      		return;
      		
        var text = t ? t : v; 

       	this.lastSelectionText = text;
        
        if(this.hiddenField){
            this.hiddenField.dom.value = v;
        }
        BI.ComboTree.superclass.setValue.call(this, text);
        this.value = v; 
    },
    
    
    
    // private
	restrictListHeight : function(){
        
        if (!this.tree)
        	return;
        	
        var h = this.tree.innerCt.getHeight();
 
 		// restrict height if tree higher than max-height
        if (h >= this.maxHeight) {
        	this.tree.getEl().setHeight(this.maxHeight);
      	}
      	else { 
        	this.tree.getEl().setHeight(this.tree.innerCt.getHeight());
      	}
      
      	// tree must be smaller than this.innerList due to its border
      	this.tree.getEl().setWidth(this.width - 2);      
    },
	

   
    isExpanded : function(){
        return this.list && this.list.isVisible();
    },


    // private
    validateBlur : function(){
    	return !this.list || !this.list.isVisible();
    },


    collapse : function(){
        if(!this.isExpanded()){
            return;
        }
        
        this.list.hide();
        
        Ext.get(document).un("mousewheel", this.collapseIf, this);
        Ext.get(document).un("mousedown",  this.collapseIf, this);
        
        this.keyNav.disable();
        this.fireEvent("collapse", this);
    },


    collapseIf : function(e){    
        if(!e.within(this.el) && !e.within(this.container) && !e.within(this.list) && !e.within(this.innerList)){
            this.collapse();
        }
    },

    
    expand : function(){
       	
        if(this.isExpanded()){
            return;
        }
                
        this.list.alignTo(this.wrap, this.listAlign);
       	      
        this.list.show();
        
        this.restrictListHeight();
                
        // collapse on mouse activity outside the expanded tree
        Ext.get(document).on("mousewheel", this.collapseIf, this);
        Ext.get(document).on("mousedown",  this.collapseIf, this);
        this.keyNav.enable();
        this.fireEvent("expand", this);
    },

    
    onTriggerClick : function(){
  
        if(this.disabled){
            return;
        }
        
        this.el.focus();
        
        if(this.isExpanded()) {
            this.collapse();
        }else {
            this.expand();
        }
    }
});
