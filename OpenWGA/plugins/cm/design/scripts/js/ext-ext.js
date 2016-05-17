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
 * Patches for Ext 1.1
 */

Ext.SplitLayoutRegion.prototype.pauseAutoHide = function(pause){
	
	if(!this.collapsed)
		return;
	
	if(pause){
		this.clearAutoHide();
		this.slideInAfterPause = false;
	    if(this.autoHide !== false){
			this.autoHideHd = {
	            "mouseout": function(e){
	                if(!e.within(this.el, true)){
						this.slideInAfterPause=true;
	                }
	            },
	            "mouseover" : function(e){
	                if(e.within(this.el, true)){
						this.slideInAfterPause=false;
	                }
	            },
	            scope : this
	        };
	    }
		this.el.on(this.autoHideHd);
	}
	else{
		if(this.slideInAfterPause)
			this.slideIn();
		this.clearAutoHide();
		this.autoHideHd=null;
		this.initAutoHide();
	}
}

/*
 * This patch corrects grid drag&drop
 * added missing call of onStartDrag()
 * also correctes wrong argument list in original source.
 */
Ext.grid.GridDragZone.prototype.onInitDrag = function(x,y){
	// code copy from GridDD.js:
    var data = this.dragData;
    this.ddel.innerHTML = this.grid.getDragDropText();
    this.proxy.update(this.ddel);
    // fire start drag?
    // 
    // added to code: Yes please:
    this.onStartDrag(x, y);
}


/*
 *
 *
 */
Ext.override(Ext.Resizable, {
    onMouseMove : function(e){
        if(this.enabled){
            try{// try catch so if something goes wrong the user doesn't get hung

            if(this.resizeRegion && !this.resizeRegion.contains(e.getPoint())) {
            	return;
            }

            //var curXY = this.startPoint;
            var curSize = this.curSize || this.startBox;
            var x = this.startBox.x, y = this.startBox.y;
            var ox = x, oy = y;
            var w = curSize.width, h = curSize.height;
            var ow = w, oh = h;
            var mw = this.minWidth, mh = this.minHeight;
            var mxw = this.maxWidth, mxh = this.maxHeight;
            var wi = this.widthIncrement;
            var hi = this.heightIncrement;

            var eventXY = e.getXY();
            var diffX = -(this.startPoint[0] - Math.max(this.minX, eventXY[0]));
            var diffY = -(this.startPoint[1] - Math.max(this.minY, eventXY[1]));

            var pos = this.activeHandle.position;

            switch(pos){
                case "east":
                    w += diffX;
                    w = Math.min(Math.max(mw, w), mxw);
                    break;
                case "south":
                    h += diffY;
                    h = Math.min(Math.max(mh, h), mxh);
                    break;
                case "southeast":
                    w += diffX;
                    h += diffY;
                    w = Math.min(Math.max(mw, w), mxw);
                    h = Math.min(Math.max(mh, h), mxh);
                    break;
                case "north":
                    diffY = this.constrain(h, diffY, mh, mxh);
                    y += diffY;
                    h -= diffY;
                    break;
                case "west":
                    diffX = this.constrain(w, diffX, mw, mxw);
                    x += diffX;
                    w -= diffX;
                    break;
                case "northeast":
                    w += diffX;
                    w = Math.min(Math.max(mw, w), mxw);
                    diffY = this.constrain(h, diffY, mh, mxh);
                    y += diffY;
                    h -= diffY;
                    break;
                case "northwest":
                    diffX = this.constrain(w, diffX, mw, mxw);
                    diffY = this.constrain(h, diffY, mh, mxh);
                    y += diffY;
                    h -= diffY;
                    x += diffX;
                    w -= diffX;
                    break;
               case "southwest":
                    diffX = this.constrain(w, diffX, mw, mxw);
                    h += diffY;
                    h = Math.min(Math.max(mh, h), mxh);
                    x += diffX;
                    w -= diffX;
                    break;
            }

            var sw = this.snap(w, wi, mw);
            var sh = this.snap(h, hi, mh);
            if(sw != w || sh != h){
                switch(pos){
                    case "northeast":
                        y -= sh - h;
                    break;
                    case "north":
                        y -= sh - h;
                        break;
                    case "southwest":
                        x -= sw - w;
                    break;
                    case "west":
                        x -= sw - w;
                        break;
                    case "northwest":
                        x -= sw - w;
                        y -= sh - h;
                    break;
                }
                w = sw;
                h = sh;
            }

            if(this.preserveRatio){
                switch(pos){
                    case "southeast":
                    case "east":
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        w = ow * (h/oh);
                       break;
                    case "south":
                        w = ow * (h/oh);
                        w = Math.min(Math.max(mw, w), mxw);
                        h = oh * (w/ow);
                        break;
                    case "northeast":
                        w = ow * (h/oh);
                        w = Math.min(Math.max(mw, w), mxw);
                        h = oh * (w/ow);
                    break;
                    case "north":
                        var tw = w;
                        w = ow * (h/oh);
                        w = Math.min(Math.max(mw, w), mxw);
                        h = oh * (w/ow);
                        x += (tw - w) / 2;
                        break;
                    case "southwest":
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        var tw = w;
                        w = ow * (h/oh);
                        x += tw - w;
                        break;
                    case "west":
                        var th = h;
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        y += (th - h) / 2;
                        var tw = w;
                        w = ow * (h/oh);
                        x += tw - w;
                       break;
                    case "northwest":
                        var tw = w;
                        var th = h;
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        w = ow * (h/oh);
                        y += th - h;
                         x += tw - w;
                       break;

                }
                
               
            }
            
			
			if(this.resizeRegion && !this.resizeRegion.contains(new Ext.lib.Region(y, x+w, y+h, x))) {
				return;
			}
		
            
            this.proxy.setBounds(x, y, w, h);
            if(this.dynamic){
                this.resizeElement();
            }
            }catch(e){}
        }
    }
});

/*
 * Ext.View
 * correct multi selection philosophy
 * - clear selection and reselect if click without SHIFT/CTRL/CMD
 * - unselect node if CTRL/CMD and node was selected 
 */
Ext.override(Ext.View, {

    onItemClick : function(item, index, e){
        if(this.fireEvent("beforeclick", this, index, item, e) === false){
            return false;
        }
        if(this.multiSelect || this.singleSelect){
            if(this.multiSelect && e.shiftKey && this.lastSelection){
                this.select(this.getNodes(this.indexOf(this.lastSelection), index), false);
            }else{
            	if(this.multiSelect && e.ctrlKey && this.isSelected(item)){
                	// unselect:
                	this.unselect(item)
            	}
				else{            	
	                // select:
	                this.select(item, this.multiSelect && e.ctrlKey);
	                this.lastSelection = item;
	           	}
            }
            e.preventDefault();
        }
        return true;
    },

	unselect: function(item){
    	for(var i=0; i<this.selections.length; i++){
    		var node = this.selections[i];
    		if(node==item){
    			this.selections.splice(i,1);	// remove item
    			Ext.fly(node).removeClass(this.selectedClass);
    			this.fireEvent("selectionchange", this, this.selections);
    			break;
    		}
    	}
	},

    select : function(nodeInfo, keepExisting, suppressEvent){
        if(nodeInfo instanceof Array){
            if(!keepExisting){
                this.clearSelections(true);
            }
            for(var i = 0, len = nodeInfo.length; i < len; i++){
                this.select(nodeInfo[i], true, true);
            }
            if(!suppressEvent){
                this.fireEvent("selectionchange", this, this.selections);
            }
        }
        else{
            var node = this.getNode(nodeInfo);
            if(node){
                if(!keepExisting){
                    this.clearSelections(true);
                }                
                if(this.fireEvent("beforeselect", this, node, this.selections) !== false){
                    Ext.fly(node).addClass(this.selectedClass);
                    this.selections.push(node);
                    if(!suppressEvent){
                        this.fireEvent("selectionchange", this, this.selections);
                    }
                }
            }
        }
    }

});
