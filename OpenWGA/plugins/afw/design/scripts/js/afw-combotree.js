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
AFW.ComboTree = Ext.extend(Ext.form.TriggerField, {
   

    // private
    defaultAutoCreate : {tag: "input", type: "text", size: "24", autocomplete: "off"},
    listClass: '',
    selectedClass: 'x-combo-selected',
    triggerClass : 'x-form-arrow-trigger',
    shadow:'sides',
    listAlign: 'tl-bl?',
    maxHeight: 300,
    resizable: false,
    handleHeight : 8,
    minListWidth : 170,
    minListHeight : 200,
    lazyInit : false,
    
    valueField:  'id',
    
    
    rootTitle: 'root',
    rootSelectable: false,
    
    initialRender: true,

    initComponent : function() {
    	
      	AFW.ComboTree.superclass.initComponent.call(this);
		
		this.addEvents({
			'expand' : true,
			'collapse' : true,
			'beforeselect' : true,
			'select' : true
		});
		if (!this.tree) {
						
			this.tree = new Ext.tree.TreePanel({
				border: false,
				loader: this.loader,
		        autoScroll:true,
		        animate:false,
		        autoHeight: true,
		        autoWidth: false,
		        width: this.width || this.minListWidth,
		        layout: 'fit'
			});
			
			this.root = new Ext.tree.AsyncTreeNode({
		        id: this.rootId || 'root',
		        text: this.rootTitle || 'root',
		        draggable: false     
		    });		    
			
			
			this.tree.on("append", function(tree, parentnode, newnode) {				
				newnode.text = unescape(newnode.text);
				if(!newnode.attributes.has_children){
					newnode.render();
					newnode.expand(false);
				}
				if (newnode.id == this.defaultValue) {
					newnode.select();
					this.setValue(this.defaultValue);
				}
			}, this);
			
			this.tree.setRootNode(this.root);
			
			if (this.defaultValue == this.rootId) {
				this.setValue(this.rootId);
			}
			/*if (this.defaultPath) {
				this.tree.selectPath(this.defaultPath, 'id',function(bSuccess, oSelNode) {
	
				}, this);
			}*/
    	}
    }, 

    // private
    onRender : function(ct, position){
         
        AFW.ComboTree.superclass.onRender.call(this, ct, position);
       
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
	    	this.trigger.setX(this.el.getX() + this.el.getWidth());
	    }
		
	    if(!this.lazyInit){
	        this.initList();
	    }else{
	        this.on('focus', this.initList, this, {single: true});
   		}
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
			
			this.tree.render(this.innerList);
			
			if (!this.defaultPath && this.defaultValue) {
				//necessary as tree will not know whether nodes are leafs or not otherwise
				this.tree.expandAll();
			}
			
			this.tree.on('click', this.onSelect, this);
			
			// list needs to be resized when nodes are collapsed/expanded		
    		this.tree.on('expandnode', function() {
    			this.restrictListSize();
    		}, this);
	
			this.tree.on('collapsenode', function() {
    			this.restrictListSize();
    		}, this);
	
			if (this.defaultPath) {
				this.setPath(this.defaultPath);
			}
			
			this.treeKeyNav = new Ext.KeyNav(this.tree.el, {
			    "tab" : function(e){
			       var node = this.tree.getSelectionModel().getSelectedNode();
			       if (node) {
			       		if (node.isExpanded())	
			       			node.collapse();
			       		else {
			       			node.expand();
			       		}
			       }
			    },
			    "esc" : function(e){
			       this.collapse();
			    },
			    scope : this
			});
			
			this.restrictListSize();
   		}
    },


    // private
    initEvents : function(){
        AFW.ComboTree.superclass.initEvents.call(this);
		
        this.keyNav = new Ext.KeyNav(this.el, {
            "up" : function(e){
                this.inKeyMode = true;
                this.selectPrev();
            },

            "down" : function(e){
                if(!this.isExpanded()){
                    this.onTriggerClick();
                }else{
                    this.inKeyMode = true;
                    this.selectNext();
                }
            },

            "enter" : function(e){
            	
            },

            "esc" : function(e){
            	this.collapse();
            },

            scope : this,

            doRelay : function(foo, bar, hname){
                if(hname == 'down' || this.scope.isExpanded()){
                   return Ext.KeyNav.prototype.doRelay.apply(this, arguments);
                }
                return true;
            },

            forceKeyDown : true
        });
    },

    onDestroy : function(){
       
        if(this.list){
            this.list.destroy();
        }
        
        if (this.tree) {
        	this.tree.destroy();
        }

        AFW.ComboTree.superclass.onDestroy.call(this);
    },
	
	
    // private
    fireKey : function(e){
        if(e.isNavKeyPress() && !this.isExpanded() && !this.delayedCheck){
            this.fireEvent("specialkey", this, e);
        }
    },

  

    // private
    onEnable: function(){
        AFW.ComboTree.superclass.onEnable.apply(this, arguments);
        if(this.hiddenField){
            this.hiddenField.disabled = false;
        }
    },

    // private
    onDisable: function(){
        AFW.ComboTree.superclass.onDisable.apply(this, arguments);
        if(this.hiddenField){
            this.hiddenField.disabled = true;
        }
    },


    // private
    onSelect : function(node, e){       
        if (!this.rootSelectable && node == this.tree.getRootNode())
        	return; 
        
        if (this.fireEvent('beforeselect', this, node, e) !== false) {
	        this.setValue(node.attributes[this.valueField]);
	        this.collapse();
	        this.fireEvent('select', this, node, e);
        }
    },
    

    /**
     * Returns the currently selected field value or empty string if no value is set.
     * @return {String} value The selected value
     */
    getValue : function(){
        if(this.valueField){
            return typeof this.value != 'undefined' ? this.value : '';
        }else{
            return AFW.ComboTree.superclass.getValue.call(this);
        }
    },

    /**
     * Clears any text/value currently set in the field
     */
    clearValue : function(){
        if(this.hiddenField){
            this.hiddenField.value = '';
        }
        this.setRawValue('');
        this.lastSelectionText = '';
        this.applyEmptyText();
        this.value = '';
    },

   
   	repairString : function(str) {
   		var temp_div = document.createElement('div');
   		temp_div.innerHTML = str.replace(/>/g, "&gt;").replace(/</g, "&lt;");
   		return temp_div.firstChild?temp_div.firstChild.nodeValue:'';
   	},
	     
    setValue : function(v){
        var text = v;
        if(this.valueField){
            
            var node = this.tree.getNodeById(v);
			
            if(node){
                text = this.repairString(node.attributes[this.displayField]);
            }else if(this.valueNotFoundText !== undefined){
                text = this.valueNotFoundText;
            }
        }
        this.lastSelectionText = text;
        
        if(this.hiddenField){
            this.hiddenField.dom.value = v;
        }
        AFW.ComboTree.superclass.setValue.call(this, text);
        this.value = v; 
    },
    
    
    
    setPath : function(path) {
    	var me = this;
    	this.tree.selectPath(path, null, function(success, selNode) {
    		if (success) me.setValue(selNode.id);  
    	});
    },
    
    // private
	restrictListSize : function(){
       
        this.innerList.dom.style.height = '';
        var inner = this.innerList.dom;
        var pad = this.list.getFrameWidth('tb')+(this.resizable?this.handleHeight:0)+this.assetHeight;
        var h = Math.max(inner.clientHeight, inner.offsetHeight, inner.scrollHeight, this.minListHeight);
        var ha = this.getPosition()[1]-Ext.getBody().getScroll().top;
        var hb = Ext.lib.Dom.getViewHeight()-ha-this.getSize().height;
        var space = Math.max(ha, hb, this.minHeight || 0)-this.list.shadow.offset-pad-2;
        h = Math.min(h, space, this.maxHeight);
        
        w = this.el.getWidth() + this.trigger.getWidth();
        
        this.innerList.setWidth(w);
        this.list.setWidth(w);
        this.tree.getTreeEl().setWidth(w);
        
        
        this.innerList.setHeight(h);
        this.tree.getTreeEl().setHeight(h);
        this.list.beginUpdate();
        this.list.setHeight(h+pad);
        this.list.alignTo(this.el, this.listAlign);
        this.list.endUpdate();
    },
	

    /**
     * Returns true if the dropdown list is expanded, else false.
     */
    isExpanded : function(){
        return this.list && this.list.isVisible();
    },


    // private
    selectNext : function(){
    	if (!this.tree)
    		return;
    		
        var selectionModel = this.tree.getSelectionModel();
        
        selectionModel.selectNext();
        
    },

    // private
    selectPrev : function(){
       	if (!this.tree)
    		return;
    		
        var selectionModel = this.tree.getSelectionModel();
        
        selectionModel.selectPrevious();
    },


    // private
    validateBlur : function(){
    	return !this.list || !this.list.isVisible();
    },


    /**
     * Hides the dropdown list if it is currently expanded. Fires the 'collapse' event on completion.
     */
    collapse : function(){
        if(!this.isExpanded()){
            return;
        }
        
        this.list.hide();
        
        Ext.getDoc().un('mousewheel', this.collapseIf, this);
        Ext.getDoc().un('mousedown', this.collapseIf, this);
        
        
        this.fireEvent('collapse', this);
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
        
        Ext.getDoc().on('mousewheel', this.collapseIf, this);
        Ext.getDoc().on('mousedown', this.collapseIf, this);
        
        this.list.setWidth(this.width || this.minListWidth);
        this.innerList.setWidth(this.width || this.minListWidth);
        
        var child = Ext.get(this.innerList.dom.firstChild);
        var grandchild =  Ext.get(child.dom.firstChild);
        var greatgrandchild = Ext.get(grandchild.dom.firstChild);
        
        child.setWidth(this.width || this.minListWidth);
        grandchild.setWidth(this.width || this.minListWidth);
		greatgrandchild.setWidth((this.width || this.minListWidth) - 2);
		
		var node = this.tree.getNodeById(this.value);
		if (node) {	
			node.select();
		}
				
		this.tree.focus();				
        
        this.fireEvent('expand', this);
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
