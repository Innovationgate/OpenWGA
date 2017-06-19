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
APPUI=function(){
	
	// overwrite WGA default ajax action
	var wga_ajax_action = WGA.ajax.action;
	function ajaxaction(actiondef, msg, graydiv){
		if(actiondef.mode!='norefresh'){ 
			APPUI.progressbar.show((msg||"loading") + " ...");
			document.body.style.cursor="wait";
			WGA.portlet.registerObject(actiondef.id, {
				destroy: function(){
					APPUI.progressbar.hide();
					document.body.style.cursor="";
				}
			});
		}
		actiondef.graydiv=graydiv||false;
		wga_ajax_action(actiondef);
	}
	WGA.ajax.action = ajaxaction
	
	return{

		showHideElement: function(id, cb){
			var el = Ext.get(id);
			el.setVisibilityMode(Ext.Element.DISPLAY);
			
			if(el.isVisible()){
				if(Ext.isIE){
					el.setStyle("display", "none");
				}
				else el.slideOut("t", {
					duration: .25,
					callback: function(){
						if(cb)
							cb(false)
					}
				});
			}
			else {
				el.removeClass("x-hidden");
				if(Ext.isIE){
					el.setStyle("display", "block");
				}
				else el.slideIn("t", {
					duration: .25,
					callback: function(){
						if(cb)
							cb(true)
					}
				})
			}
		}
		
		,progressbar: function(){
			var pbar = new Ext.ProgressBar();
			var messages=[];
			
			return{
				show: function(text){
					var pbar_el = Ext.get("progressbar");
					if(!pbar_el)
						return;
					if(!pbar.rendered){								
						pbar.render(pbar_el);
					}
					else pbar.show();
					if(messages.length==0)
						pbar.wait();
					messages.unshift(text)
					pbar.updateText(text);
				}
				
				,hide: function(){
					if(pbar.rendered){
						messages.shift();
						if(messages.length==0)
							pbar.reset(true);
						else pbar.updateText(messages[0]);
					}
				}
				
				// debug:
				,bar: pbar
				,msg: messages
			}
			
		}()
		
		,switchApp: function(app, uid){
			var params = {appkey:app, uid:uid};
			WGA.event.fireEvent("PS-app-selected", "*", params)
		}
		
		,ajaxAction: ajaxaction
		
		,callAction: function(action, portletkey, msg){
			ajaxaction({
				action: action,
				id: portletkey
			}, msg, true)
		}

	}

}()
