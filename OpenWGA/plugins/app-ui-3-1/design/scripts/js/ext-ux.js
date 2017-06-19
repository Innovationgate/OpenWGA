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
/* 
 * Singleton-Class to display a self-hiding info-message
 * at center/top of the screen. 
 * Example: AFW.util.InfoWidget.msg('Title', 'Message with {0}...', 'parameter');
 */
Ext.ux.InfoWidget = function() {
    var msgCt;

    function createBox(t, s){
        return ['<div class="afw-info-msg">',
                '<h1>', t, '</h1><div>', s, '</div>',
                '</div>'].join('');		
    }
    
    return {
        msg : function(title, format, pause) {
            if(!msgCt){
                msgCt = Ext.DomHelper.insertFirst(document.body, {id:'msg-div'}, true);
            }
            msgCt.alignTo(document, 't-t');
            var s = String.format.apply(String, Array.prototype.slice.call(arguments, 1));
            var m = Ext.DomHelper.append(msgCt, {html:createBox(title, s)}, true);
            m.slideIn('t').pause(pause||3).ghost("t", {remove:true});
        }
    };
}();

/*
 * Correct Drag&Drop Handler for grids
 */

Ext.override(Ext.grid.GridDragZone, {
    onInitDrag : function(e){
        var data = this.dragData;
        this.ddel.innerHTML = this.grid.getDragDropText();
        this.proxy.update(this.ddel);
        this.onStartDrag();
    }
})

