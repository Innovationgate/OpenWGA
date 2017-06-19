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
Page = function(){
	
	var maskel;
	
	function keyNavAction(ev) {
        var keyCode = ev.getKey();
        if(
            keyCode == 88 || // x
            keyCode == 67 || // c
            keyCode == 27
        ){
            closePage();
        }
	}
	
	function closePage(){
	
		var el=Ext.get("modal-image");	
		//el.child(".page-wrap").hide();
		el.hide({
			callback: function(){
				el.removeAllListeners();
				maskBody(false);
				el.remove();
				Ext.fly(document).un('keydown', keyNavAction);
			}
		})
	}
	
	function maskBody(mask){
		if(!maskel){
			// create one
			maskel = Ext.getBody().createChild({tag:"div"});
			maskel.setStyle({
				position: "absolute",
				zIndex: 100,
				opacity: .7,
				top: Ext.getBody().getScroll().top+"px",
				left: 0,
				width: Ext.lib.Dom.getViewportWidth()+"px",
				height: Ext.lib.Dom.getViewportHeight()+"px",
				backgroundColor: "black",
				overflow: "hidden"
			})
		}
		
		if(mask){
			Ext.getBody().setStyle("overflow", "hidden");
		}
		else {
			Ext.getBody().setStyle("overflow", "auto")
			maskel.remove();
			maskel=null;
		}
	}
	
	function showPage(url, config){
		
		var config = config||{};
		 
		maskBody(true);

		var el=Ext.get("modal-image");
		if(el==null){
			// create div to show page in:
			el = Ext.getBody().createChild({tag:"div", id:"modal-image"});
		}
		el.hide();
		el.setStyle("zIndex", 30000);

		el.setHeight(200)
		el.setWidth(200);

		//allign element:		
		el.setLeft((maskel.getWidth()-200)/2);
		el.setTop(maskel.getTop() + ((maskel.getHeight()-200)/2));
		
		el.show();		
		
		var page_type = config.type || "ajax";
		
		var t;
		if(page_type=="ajax"){
			t = new Ext.Template(
					'<div class="loading">loading ...</div>',
					'<div class="page-wrap" style="display:none;position:abslolute">',
						'<div id="popup-page-content" style="overflow:auto;background-color: silver">content goes here ...</div>',
					    '<div style="float:left" class="img-title">{title}</div>',
					    '<div style="float:right"><a href="#" class="close-img">ESC to close</a></div>',
				    '</div>'
				);
			t.append(el.dom, {title: config.title||""});
			// load content:
			var contentEl = el.child("#popup-page-content");
			WGA.ajax.request("GET", url, null, function(xmlHttpReq){
	
			    // IE does not recognize the first script tag by getElementsByTagName
			    // if there is no html:tag with content in front
			    // so add a none displayed span-tag in front of the pasted code
				var html = "<span id='ie-ajax-dummy' style='display:none'>ajax</span>" + xmlHttpReq.responseText 
				contentEl.update(html)
				
				var divTag = contentEl.dom;
				
			    // evaluate pasted javascript
			    var scripts = divTag.getElementsByTagName("script");
				if(WGA.isIE){
				    for (var i = 0; i < scripts.length; i++) {
				    	var script = scripts[i].text;
						WGA.ajax.executeScriptElement(script);
					}
				}
				else{
					var totalscripts="";
				    for (var i = 0; i < scripts.length; i++) {
						var script = scripts[i].innerHTML;
						totalscripts += "\n"+script;
					}
					window.setTimeout(totalscripts, 10);
				}
				Ext.fly("ie-ajax-dummy").remove();
				animate();
			})
		}
		else if(page_type=="movie"){
			t = new Ext.Template(
					'<div class="loading">loading ...</div>',
					'<div class="page-wrap" style="display:none;position:abslolute">',
						'<embed id="popup-page-content" autoplay="true" width="{width}" height="{height}" style="background-color: black"></embed>',
					    '<div style="float:left" class="img-title">{title}</div>',
					    '<div style="float:right"><a href="#" class="close-img">ESC to close</a></div>',
				    '</div>'
			);
			t.append(el.dom, {title: config.title||"", width:config.width, height: config.height});
			config.width+=30;
			config.height+=30;
			animate(function(embed_el){
				embed_el.dom.src=url;
				embed_el.setStyle("overflow", "auto");
			})
		}

		function animate(onFinish){	
			var padding = config.padding||[50,50];
			var w = Ext.lib.Dom.getViewportWidth() - padding[0]*2;
			var h = Ext.lib.Dom.getViewportHeight() - padding[1]*2;
			var x = padding[0];
			var y = padding[1];

			if(config.width){
				w = config.width;
				x = Math.max((Ext.lib.Dom.getViewportWidth()-w)/2, 0)
			}
			if(config.height){
				h = config.height;
				y = Ext.getBody().getScroll().top + Math.max((Ext.lib.Dom.getViewportHeight()-h)/2, 0);
			}
	
			el.shift({
				x: x,
				width: w
			}).shift({
				y: y,
				height: h,
				callback: function(){
					el.child(".loading").remove();
					var contentEl = el.child("#popup-page-content");
					contentEl.setHeight(h-30); 
					el.child(".page-wrap").show(true);
					if(onFinish)
						onFinish(contentEl);
				}
			});
			
		}

		var close = el.child(".close-img");
		if(close){
			close.on("click", function(ev){
				closePage();
				ev.stopEvent();
			})
		}		

		Ext.fly(document).on('keydown', keyNavAction);
		
	}

	return{
		show: showPage
	}
	
}()
