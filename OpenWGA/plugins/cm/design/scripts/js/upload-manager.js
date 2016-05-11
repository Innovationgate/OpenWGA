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

BI.UploadManager = function(){

	var tpl = new Ext.Template(
		'<div class="file">',
			'<div class="progress">&nbsp;</div>',
			'<div class="filename">{filename}</div>',
			'<div class="filesize">{filesize} KB</div>',
			'<div style="clear:both"></div>',
		'</div>'
	);
	
	function uploadFile(file, config){
	
		if(uploadQueue.get[file.name])
			return alert("file " + file.name + " already uploading");
		
		var queueEntry = {
			p: 0,
			filename: file.name,
			filesize: file.size
		}
		
		if(fileListEl){
			var el = queueEntry.el = tpl.append(fileListEl, {filename:file.name, filesize:(file.size/1000).toFixed(1)})
			queueEntry.p_el = Ext.get(el).child(".progress");
			Ext.get(el).addClass("uploading");
		}
		
		uploadQueue.add(file.name, queueEntry);
		
		new AFW.FileUploader(file, config.url, {
		
			urlparams: config.params
		
			,onProgress: function(p){
				queueEntry.p = p;
				if(fileListEl){
		            queueEntry.p_el.setWidth(parseInt(100*p) + "%");
		            if(p>=1)
		            	queueEntry.p_el.setStyle("background", "green");
		      	}
			}
		
			,onSuccess: function(filename){
      			if(fileListEl)
      				queueEntry.el.parentNode.removeChild(queueEntry.el);
      			uploadQueue.remove(file.name);
      			if(config.callback)
      				config.callback(filename, file.size);
			}

			,onError: function(msg){
				if(fileListEl)
      				queueEntry.p_el.setStyle("background", "red");
				uploadQueue.remove(file.name);
      			alert(msg);
			}

		})
		
	}
	
	var fileListEl = null;
	var uploadQueue = function(){
		var queue = {};
		var count = 0;
		
		return {
			add: function(key, o){
				if(!queue[key])
					count++;
				queue[key]=o;
				if(fileListEl)
					Ext.get(fileListEl).setDisplayed(true);
			},
			remove: function(key){
				delete queue[key];
				count--;
				if(!count && fileListEl)
					Ext.get(fileListEl).setDisplayed(false);
			},
			forEach: function(f){
				for(i in queue){
					f(queue[i]);
				}
			},
			get: function(key){
				return queue[key]
			},
			isEmpty: function(){
				return count==0;
			}
		}
	}()	
	
	return{
		show: function(el){
			fileListEl = el;
			Ext.get(fileListEl).setDisplayed(!uploadQueue.isEmpty());
			uploadQueue.forEach(function(q){
				var el = tpl.append(fileListEl, {filename:q.filename, filesize:(q.filesize/1000).toFixed(1)})
				var p_el = Ext.get(el).child(".progress");
				Ext.get(el).addClass("uploading");
				p_el.setWidth(parseInt(100*q.p) + "%");
	            if(q.p>=1)
	            	p_el.setStyle("background", "green");
				
				q.el = el;
				q.p_el = p_el;
			})
		},
		
		upload: uploadFile,
		
		destroy: function(){
			fileListEl = null;
		}
	}	
	
		
}();

