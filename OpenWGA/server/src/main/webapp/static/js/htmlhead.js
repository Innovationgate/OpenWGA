/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * In addition, a special exception is granted by the copyright holders of
 * OpenWGA called "OpenWGA plugin exception". You should have received a copy of
 * this exception along with OpenWGA in file COPYING. If not, see
 * <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * OpenWGA in file COPYING. If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
// htmlhead.js
// ensure wga session storage keys are created
/**
 * define Namespace for WGA
 */
WGA = function() {
	var ua = navigator.userAgent.toLowerCase();
	var isSafari = ua.indexOf("safari") > -1;
	var isWebKit = ua.indexOf("webkit") > -1;
	var isIE = ua.indexOf("msie") > -1;
	var isIE7 = ua.indexOf("msie 7") > -1;
	var isTrident = ua.indexOf("trident")>-1;
	var isIE11 = ua.indexOf("trident")>-1 && ua.indexOf("rv:11") > -1;
	var isIE12 = ua.indexOf("trident")>-1 && ua.indexOf("rv:12") > -1;
	var isGecko = !isWebKit && ua.indexOf("gecko") > -1;
	var isFirefox = ua.indexOf("firefox") > -1;
	var isFirefox3 = ua.indexOf("firefox/3") > -1;
	var isFirefox4 = ua.indexOf("firefox/4") > -1;

	var isIPhone = ua.indexOf("iphone") > -1;
	var isIPad = ua.indexOf("ipad") > -1;
	var isAndroid = ua.indexOf('android') > -1;

	function fireFoxVersion(){
		if(!isFirefox)
			return 0;
			
		var e = /firefox\/(\d+)/g
		var v = e.exec(ua);
		if(v && v.length && v.length>1)
			return Number(v[1]);
		else return 0;
	}

	function IEVersion(){
		if(isIE){
			var e = /msie (\d+)/g
			var v = e.exec(ua);
			if(v && v.length && v.length>1)
				return Number(v[1]);
			else return 0;
		}
		else if(isTrident){
			var e = /rv:(\d+)/g
			var v = e.exec(ua);
			if(v && v.length && v.length>1)
				return Number(v[1]);
			else return 0;
		}
		else return 0;
	}

	return {
		isSafari : isSafari,
		isWebKit : isWebKit,
		isIE : isIE,
		isIE7 : isIE7,
		isIE11 : isIE11,
		isIE12 : isIE12,
		isTrident: isTrident,
		IEVersion: IEVersion,
		isGecko : isGecko,
		isFirefox : isFirefox,
		isFirefox3 : isFirefox3,
		isFirefox4 : isFirefox4,
		fireFoxVersion: fireFoxVersion,
		isIPhone : isIPhone,
		isIPad : isIPad,
		isAndroud : isAndroid,
		isMobile : (isIPhone || isIPad || isAndroid)
	};

}();

WGA.responsive = {
	breakpoints:{
		medium: 0,
		large: 0
	},
	getMediaWidth: function(){
		if(window.matchMedia){
			if(window.matchMedia("(min-width:" + this.breakpoints.large + "px)").matches)
				return "large"
			if(window.matchMedia("(min-width:" + this.breakpoints.medium + "px)").matches)
				return "medium"
			else return "small"
		}
		else{
			var w = window.innerWidth || $(window).width();
			if(w<this.breakpoints.medium)
				return "small"
			if(w<this.breakpoints.large)
				return "medium"
			else return "large"
		}
	}
}

WGA.util = /**
 * @author oliver
 *
 */
/**
 * @author oliver
 *
 */
{
	showException : function(msg, e) {
		msg += "\n";
		if (e.fileName)
			msg += "file: " + e.fileName + "\n";
		if (e.lineNumber)
			msg += "line: " + e.lineNumber + "\n";

		alert(msg + "\n" + e.message);
	},

	showReloadMessage: function(msg){
		var id = "wga-reload-message"
		var div = document.getElementById(id);
		if(!div){
			div = document.createElement("div");
			div.id = "wga-reload-message";
			div.style.padding="10px";
			div.style.position="fixed";
			div.style.left=div.style.right=div.style.top=0;
			div.style.backgroundColor="gray";
			div.style.color="white";
			div.style.boxShadow="0 0 10px black";
			div.style.zIndex=10000;

			var closeButton = document.createElement("button")
			closeButton.appendChild(document.createTextNode("X"));
			closeButton.style.float="right";
			closeButton.style.margin="0";
			closeButton.style.color="darkgray";
			closeButton.style.fontWeight="normal";
			closeButton.style.border="none";
			closeButton.style.fontFamily = "sans-serif";
			closeButton.style.fontSize = "10pt";
			closeButton.style.background="transparent";
			closeButton.onmouseenter = function(ev) {
				ev.target.style.color = "white";
			}
			closeButton.onmouseleave = function(ev) {
				ev.target.style.color = "darkgray";
			}
			closeButton.onclick=function(){
				document.getElementById(id).style.display="none"
			}
			div.appendChild(closeButton);
			
			var span = document.createElement("span")
			div.appendChild(span)
			
			var reloadButton = document.createElement("button")
			reloadButton.style.margin = "10px 0 0 0";
			reloadButton.appendChild(document.createTextNode("OK"));
			reloadButton.onclick=function(){
				top.location.reload();
			}
			div.appendChild(reloadButton);
			
			var firstElement = document.body.firstElementChild
			if(firstElement)
				document.body.insertBefore(div, firstElement);
			else document.body.appendChild(div);
			reloadButton.focus();
		}
		div.children[1].innerHTML = msg + "<br>";
		div.style.display="block";
	}

};

WGA.util.getURLTypeFromClassName = function(classname) {
	var classes = classname.split(" ");
	for ( var i = 0; i < classes.length; i++) {
		var c = classes[i].split("-");
		if (c[0] == "wga" && c[1] == "urltype") {
			return c[2];
		}
	}
	return null;
};

WGA.util.getLinkInfo = function(atag) {
	// old style:
	var linktype = atag.getAttribute("linktype");
	var wgakey = atag.getAttribute("wgakey");
	if (!linktype) {
		// new style:
		// link has class="wga-urltype-<linktype>"
		linktype = WGA.util.getURLTypeFromClassName(atag.className) || "exturl";
		var path = WGA.util.decodeURI(atag.href).split("/");
		if (linktype == "file") {
			var filename = path.pop();
			var container = path.pop();
			wgakey = container + "/" + filename;
		} else if (linktype == "exturl")
			wgakey = atag.href;
		else
			wgakey = path.pop(); // last element in path
	}
	if (wgakey) {
		wgakey = wgakey.split("?")[0];
		wgakey = wgakey.split("#")[0];
	}

	return {
		type : linktype,
		key : wgakey
	};
};

WGA.util.decodeURI = function(url) {
	return decodeURI(url.replace(/\+/g, " ").replace(/%2B/g, "+"));
};

/**
 * convert given html code and create scriptlets for links and images
 * 
 * @param {String}
 *            htmltext
 * @param {Object}
 *            config (optional)
 */
WGA.util.makeScriptlets = function(htmltext, config) {

	var config = config || {};
	var document = config.document || window.document;
	var contentinfo = config.contentinfo || WGA.contentinfo;
	var contentkey = contentinfo ? contentinfo.contentkey : myContentKey; // old
																			// style
																			// BI3
																			// way
	var dbkey = contentinfo ? contentinfo.dbkey : myDbKey; // old style BI3 way

	/*
	 * internal Helper function used to convert old imgage urls into old
	 * scriptlet code
	 */
	function convertWGAKeysToScriptlets(htmltext) {
		// console.log("convertWGAKeysToScriptlets", htmltext);
		var server_host = self.location.hostname;
		var server_port = self.location.port;
		var server_protocol = self.location.protocol;

		var result = htmltext;

		// Replace contentKey

		// var keyRegExp = new RegExp(editor.contentkey.replace( /\./g, "\\.")
		// ,'gi');
		// B00004486: problems with notes-migrated RTF-s
		// changed the regExp to ignore version
		var k = contentkey.split(".");
		k.pop(); // remove last part (version)
		k.push("\\d+"); // add wildcard (any number) for version

		var keyRegExp = new RegExp(k.join("\\.") + "/", 'gi');
		result = result.replace(keyRegExp, "{%$key%}/");

		// Replace absolute URLs by relative URLs
		var tmpPort = server_port;
		if (tmpPort && tmpPort != null && tmpPort != '') {
			tmpPort = ":" + tmpPort;
		}

		// replace absolute URL-s containing dbkey:
		keyRegExp = new RegExp(server_protocol + "\/\/" + server_host + tmpPort
				+ WGA.contextpath + "/" + dbkey, 'gi');
		result = result.replace(keyRegExp, "../../../{%$db:dbkey%}");

		// replace nearly absolute URL-s (without protocoll:server:port)
		// containing dbkey:
		keyRegExp = new RegExp(WGA.contextpath + "/" + dbkey, 'gi');
		result = result.replace(keyRegExp, "../../../{%$db:dbkey%}");

		// Replace dbKeys in relative URLs entered by user ( MUST be executed
		// AFTER absolute URLs. )
		keyRegExp = new RegExp("/" + dbkey.replace(/\./g, "\\.") + "/", 'gi');
		result = result.replace(keyRegExp, "/{%$db:dbkey%}/");

		// console.log("result-end", result);
		return result;
	}

	// use browsers dom as HTML parser:
	var el = document.createElement("div");
	el.innerHTML = htmltext;

	// convert link urls to scriptlets
	var links = el.getElementsByTagName("a");
	for ( var i = 0; i < links.length; i++) {
		var link = links.item(i);
		var linktype = link.getAttribute("linktype");
		if (linktype) {
			// old style:
			// convert to new style and remove old attributes:
			link.removeAttribute("linktype");
			link.removeAttribute("wgakey");
			var c = link.className;
			c = c.replace(/wga-urltype-\w+ */, "");
			link.className = c + (c ? " " : "") + "wga-urltype-" + linktype;
		} else {
			// new style:
			// link has class="wga-urltype-<linktype>"
			linktype = WGA.util.getLinkInfo(link).type;
		}

		var path = WGA.util.decodeURI(link.href).split("/");
		switch (linktype) {
		/*
		 * Don't directly write URL to link.href because FF3 will encode this
		 * URL in this case So we write the URL to an Attribute "wga:href" that
		 * will later be removed by a regexp
		 */
		case "int":
			// calc structkey
			var structkey = path.pop();
			link.setAttribute("wga:href", "{%!contenturl:" + structkey + "%}");
			link.removeAttribute("href");
			break;
		case "intname":
			var uname = path.pop();
			link.setAttribute("wga:href", "{%!namedcontenturl:" + uname + "%}");
			link.removeAttribute("href");
			break;
		case "file":
			// calc filename and container
			var filename = path.pop();
			var container = path.pop();
			link.setAttribute("wga:href", "{%!fileurl:" + container + ","
					+ filename + "%}");
			link.removeAttribute("href");
			break;
		case "intfile":
			// calc filename
			var filename = path.pop();
			link.setAttribute("wga:href", "{%!fileurl:" + filename + "%}");
			link.removeAttribute("href");
			break;
		}

	}

	// convert IMG urls to scriptlets
	var imgs = el.getElementsByTagName("img");
	for ( var i = 0; i < imgs.length; i++) {
		var img = imgs.item(i);
		var urltype = WGA.util.getURLTypeFromClassName(img.className);
		var path = WGA.util.decodeURI(img.src).split("/");
		switch (urltype) {
		case "file":
			// calc filename and container
			var filename = path.pop();
			var container = path.pop();
			img.setAttribute("wga:src", "{%!imgurl:" + container + ","
					+ filename + "%}");
			img.removeAttribute("src");
			break;
		case "intfile":
			// calc filename
			var filename = path.pop();
			img.setAttribute("wga:src", "{%!imgurl:" + filename + "%}");
			img.removeAttribute("src");
			break;
		case null:
			/*
			 * B00004D6A WGA 4.1.1: convert only old style images instead of
			 * global convertWGAKeysToScriptlets() old style img-tags from wga
			 * 4.0:
			 */
			img.setAttribute("wga:src", convertWGAKeysToScriptlets(img.src));
			img.removeAttribute("src");
			break;
		}
	}

	var htmltext = el.innerHTML;
	htmltext = htmltext.replace(/wga:href="([^"]*)"/g, 'href="$1"');
	htmltext = htmltext.replace(/wga:src="([^"]*)"/g, 'src="$1"');

	return htmltext;
};

WGA.util.maskElement = function(el) {

	/**
	 * Helper function to stop events in gekko browser
	 */
	function __moz_stopPropagation(event) {
		event.stopPropagation();
	}

	try {
		if (document.all) {
			// for IE and Opera:
			el.style.width = el.offsetWidth; // IE and Opera needs a
												// "layout-attribute" like width
			el.style.filter = "alpha(opacity:20)";
			// capture all mouse events until div is reloaded.
			if (!window.opera) {
				el.setCapture(true); // IE: capture all mouse events
			} else {
				// for opera
				el.style.opacity = 0.2;
				el.addEventListener('click', __moz_stopPropagation, true);
				el.addEventListener('mouseover', __moz_stopPropagation, true);
			}
		} else {
			// for mozilla:
			el.style.opacity = 0.2;
			if (el.addEventListener) {
				el.addEventListener("click", __moz_stopPropagation, true);
				el.addEventListener("mouseover", __moz_stopPropagation, true);
			}
		}
	} catch (e) {
		WGA.util.showException("mask-element", e);
	}
};
WGA.util.unmaskElement = function(el) {
};


/**
 * Generates a schedule interval for exponential backoff
 * @param {Number} attempts - Number of attempts of the executed functionality. Major numbers generate major intervals.
 * @param {Number} max - Maximum milliseconds for the interval
 */
WGA.util.generateInterval = function(attempts, max) {
  var maxInterval = (Math.pow(2, attempts) - 1) * 1000;
  
  if (maxInterval > max) {
    maxInterval = max;
  }
  
  return Math.random() * maxInterval; 
}


/**
 * Load a label from a label property object
 * Uses the browser locales to find the best matching one.
 * @param labels Object containing a label text for multiple language, keyed by the languages
 */
WGA.util.label = function(labels, defaultLanguage) {
	
	// Full locale codes
	if(navigator.languages){
		for (var idx=0; idx < navigator.languages.length ; idx++) {
			var label = labels[navigator.languages[idx]];
			if (label) {
				return label;
			}
		}
		
		// Only language codes
		for (var idx=0; idx < navigator.languages.length ; idx++) {
			var locale = navigator.languages[idx];
			var subIdx = locale.indexOf("_");
			if (subIdx != -1) {
				var language = locale.substring(0, subIdx);
				var label = labels[language];
				if (label) {
					return label;
				}
			}
		}
	}
	else if(navigator.language){
		var label = labels[navigator.language];
		if (label) {
			return label;
		}
		var subIdx = navigator.language.indexOf("_");
		if (subIdx != -1) {
			var language = navigator.language.substring(0, subIdx);
			var label = labels[language];
			if (label) {
				return label;
			}
		}
	}

	// Fallback: Default language
	if (defaultLanguage) {
		return labels[defaultLanguage];
	}
	
	return null;
	
}

/**
 * Generates content urls, for usage by other js functions
 * 
 * @param {String}
 *            key
 */
WGA.buildContentURL = function(key) {

	if (requestType == 'statictml' || requestType == 'null') {
		return "";
	}
	if (mediaKeyMode == 1) {
		if (key != null) {
			return WGPPath + "/" + myLayout + "/" + key + "." + myMedium;
		} else {
			return WGPPath + "/" + myLayout + "." + myMedium;
		}
	} else {
		if (key != null) {
			return WGPPath + "/" + myMedium + "/" + myLayout + "/" + key;
		} else {
			return WGPPath + "/" + myMedium + "/" + myLayout;
		}
	}
};

/**
 * Module to register onload functions in WGA. onload-s are attached to the
 * window.onload-event and are called after Ajax calls
 */
WGA.onload = function() {

	var callbacks = [];

	return {

		register : function(f) {
			callbacks.push(f);
			return f;
		},

		reset : function() {
			callbacks = [];
		},

		/**
		 * call all registered onload-s and deregister functions
		 */
		execute : function() {
			while (callbacks.length > 0)
				callbacks.shift()();
			
			WGA.pageLoaded = true;
		}
	};
}();

// attach to window.onload:
if (window.addEventListener)
	window.addEventListener("load", WGA.onload.execute, true);
else if (window.attachEvent)
	window.attachEvent("onload", WGA.onload.execute);

/**
 * Form submit function register. Registered functions are called in callAction
 * before submitting the form The form will only be submitted if the function
 * returns true;
 */
WGA.b4submit = function() {
	var callbacks = {};

	return {

		register : function(formid, f) {
			if (!callbacks[formid])
				callbacks[formid] = [];
			callbacks[formid].push(f);
			return f;
		},

		onsubmit : function(form) {
			if (!form || form.id == "" || !callbacks[form.id])
				return true;

			for ( var i = 0; i < callbacks[form.id].length; i++) {
				var f = callbacks[form.id][i];
				if (!f(form))
					return false; // onsubmit-function returned false
			}

			return true;
		},

		reset : function(formid) {
			callbacks[formid] = [];
		}
	};
}();

/**
 * Parses a JavaScript action link and divides it up into it's parts
 */
WGA.parseActionLink = function(actionLink) {

	var elements = actionLink.split("/");
	for ( var i = 0; i < elements.length; i++) {
		if (elements[i] == "##NULL##") {
			elements[i] = null;
		}
	}

	switch (elements.length) {

	case 1:
		return {
			versionCompliance : null,
			formID : null,
			portletKey : null,
			actionLink : elements[0]
		};

	case 2:
		return {
			versionCompliance : null,
			formID : elements[0],
			portletKey : null,
			actionLink : elements[1]
		};

	case 3:
		return {
			versionCompliance : null,
			formID : elements[0],
			portletKey : elements[1],
			actionLink : elements[2]
		};

	case 4:
		return {
			versionCompliance : elements[0],
			formID : elements[1],
			portletKey : elements[2],
			actionLink : elements[3]
		};

	default: {
		alert("Error calling Action. Invalid number of action link parts:  "
				+ elements.length);
	}

	}

};

WGA.isVersionComplianceSupporting = function(version, feature) {

	if (!version) {
		return false;
	}

	var parts = version.split(".");
	var major = parseInt(parts[0]);
	var minor = parseInt(parts[1]);

	if (feature == "encodeUrlParamsDefault") {
		if (major > 6) {
			return true;
		} else if (major == 6 && minor >= 1) {
			return true;
		} else {
			return false;
		}
	}

	if (feature == "keepUrlParamsDefault") {
		return (major >= 6);
	}

	return false;

};

/**
 * generate action URL and open page or submit the form if action comes from a
 * form
 * 
 * @param {String}
 *            actionLink
 * @param {Object}
 *            URL params
 */
WGA.callAction = function(actionLink, params) {

	// Catch certain legacy calls to callAction() where the 2nd param was the
	// portlet key.
	if (params && typeof (params) != "object") {
		params = undefined;
	}

	return WGA.action({
		action : actionLink,
		params : params
	});
};

/**
 * generate action URL and open page or submit the form if action comes from a
 * form
 * 
 * @param {Object}
 *            action def .action - The action link .params - URL params
 *            .keepParams - Whether the URL params of the initial request should
 *            be kept. Defaults to true.
 */
WGA.action = function(actionDef) {

	var link = WGA.parseActionLink(actionDef.action);
	var urlParams = (actionDef.params != undefined ? actionDef.params : {});

	// Fetch local portlet states
	var portletStates = [];
	if (link.portletKey) {
		WGA.portlet.collectPortletStates(portletStates, link.portletKey);
		if (actionDef.portletEvent) {
			if (actionDef.portletEvent.index) {
				urlParams["$portletEvent"] = actionDef.portletEvent.index;
			}
			else {
				urlParams["$portletEventName"] = actionDef.portletEvent.name;
			}
		}
	}
	
	// Determine URL params behaviour
	var keepParams = (actionDef.keepParams == undefined ? WGA
			.isVersionComplianceSupporting(link.versionCompliance,
					"keepUrlParamsDefault") : actionDef.keepParams);
	var encodeParams = (actionDef.encodeParams == undefined ? WGA
			.isVersionComplianceSupporting(link.versionCompliance,
					"encodeUrlParamsDefault") : actionDef.encodeParams);

	// Inside a portlet but without form. To transmit portlet states we have to perform a POST with a customly created form 
	if (link.portletKey && !link.formID && portletStates.length > 0) {
		
		    var form = document.createElement("form");
			var qs = WGA.toQueryString([], keepParams, encodeParams);
			form.setAttribute("action", location.pathname);
		    form.setAttribute("method", "POST");
		    form.setAttribute("accept-charset", "UTF-8");
		    urlParams["$action"] = link.actionLink;
		    urlParams['$portletStates'] = portletStates.join("\n");
		    
		    for(var key in urlParams) {
		        if(urlParams.hasOwnProperty(key)) {
		            var hiddenField = document.createElement("input");
		            hiddenField.setAttribute("type", "hidden");
		            hiddenField.setAttribute("name", key);
		            hiddenField.setAttribute("value", urlParams[key]);

		            form.appendChild(hiddenField);
		         }
		    }
            
		    document.body.appendChild(form);
		    form.submit();
		
	}
	
	// Regular GET action
	else if (link.formID == null) {
		urlParams["$action"] = link.actionLink;
		var url = location.pathname + "?" + WGA.toQueryString(urlParams, keepParams, encodeParams);
		location.replace(url);
	}

	// Regular POST action
	else {
		var form = document.forms[link.formID];
		if (!form || !form.$formaction) {
			alert("Error while calling action. TMLForm \"" + link.formID
					+ "\" not found.");
			return false;
		}
		
		form.$formaction.value = link.actionLink;
		if (portletStates.length > 0) {
			form.$portletstates.value = portletStates.join("\n");
		}
		
		var qs = WGA.toQueryString(urlParams, keepParams, encodeParams);
		form.action = location.pathname + (qs ? "?" + qs : "");
		if (WGA.b4submit.onsubmit(form))
			form.submit();
	}
	return true;

};

WGA.getQueryString = function(win) {
	var qs = win.location.search;
	var map = {};

	function decode(s) {
		try {
			return decodeURIComponent(s).replace(/\r\n|\r|\n/g, "\r\n");
		} catch (e) {
			return "";
		}
	}
	
	if (qs.length > 1) {
		qs = qs.substr(1);

		qs.replace(/([^&]+)/g, function(match, param) {

			var name = param;
			var value = null;
			var equalPos = param.indexOf("=");
			if (equalPos != -1) {
				name = param.substring(0, equalPos);
				value = param.substring(equalPos + 1);
			}

			name = decode(name);
			if (value != null) {
				value = decode(value);
			}
			if (name.length > 0) {
				map[name] = value;
			}
		});
	}
	return map;
}

/**
 * Escapes reserved characters in a query part. Does not escape Non-ASCII
 * characters as we rely on the browser to choose the correct encoding
 * automatically.
 */
WGA.escapeQueryPart = function(part) {
	return encodeURIComponent(part);
};

WGA.isObjectType = function(o, type) {
	return Object.prototype.toString.call(o) === '[object '+ type + ']';
}

/**
 * Generate a query string containing the object properties
 */
WGA.toQueryString = function(customParams, keepParams, encodeParams, removeParams) {

	var params = {};

	// Push current URL params
	if (keepParams == undefined || keepParams == true) {
		var currentParams = WGA.getQueryString(window);
		for ( var key in currentParams) {
			if (currentParams[key] != null) {
				params[String(key)] = String(currentParams[key]);
			} else {
				params[String(key)] = null;
			}
		}
	}

	// Push given params
	for (var key in customParams) {
		params[String(key)] = customParams[key];
	}

	// Push into array. Remove if contained in removeParams object
	var paramsArray = [];
	for (var key in params) {
		if (removeParams && removeParams.hasOwnProperty(key)) {
			continue;
		}
		
		var value = params[key];
		WGA.pushQueryParameter(key, value, paramsArray, encodeParams);
	}

	// Join query string
	// console.log(WGA.toQueryString, paramsArray.join("&"))
	return paramsArray.join("&");
};

WGA.pushQueryParameter = function(key, value, paramsArray, encodeParams) {
	
	if (value === undefined || value === null) {
		if (encodeParams) {
			key = WGA.escapeQueryPart(key);
		}
		paramsArray.push(key);
	}
	
	else if (WGA.isObjectType(value, "Array")) {
		for (var idx=0; idx < value.length; idx++) {
			WGA.pushQueryParameter(key, value[idx], paramsArray, encodeParams);
		}
	}
	
	else {
		if (encodeParams) {
			key = WGA.escapeQueryPart(key);
		}
		if (encodeParams) {
			value = WGA.escapeQueryPart(value);
		}
		paramsArray.push(key + "=" + value);
	}

};

WGA.loadScript = function(config) {
	if (config.id && document.getElementById(config.id)) {
		// script already loaded
		if (config.onload)
			config.onload();
		return;
	}
	var head = document.getElementsByTagName('head')[0];
	var scriptElem = document.createElement('script');
	scriptElem.src = config.src;
	if (config.id)
		scriptElem.id = config.id;
	scriptElem.type = 'text/javascript';
	scriptElem.onload = config.onload;

	// IE does not know onload in script tags.
	// IE really does every thing its own way...
	scriptElem.onreadystatechange = function() {
		if (this.readyState == 'loaded')
			config.onload();
	};

	head.appendChild(scriptElem);
};

/*******************************************************************************
 * AJAX handling
 */

/**
 * Namespace for AJAX methods
 */
WGA.ajax = {

	portletFormCallbacks : {},
	runningPortlets : {},

	hasRunningPortlets : function() {
		var now = (new Date()).getTime();
		for ( var p in this.runningPortlets) {
			var portlet_time = this.runningPortlets[p].getTime();
			if (now - portlet_time > 10000) {
				delete this.runningPortlets[p];
				continue;
			}
			return true;
		}
		return false;
	}
};

/**
 * Object to register function to be called before ajax calls
 */
WGA.ajax.b4post = function() {

	var callbacks = {};

	return {
		register : function(id, f) {
			if (!callbacks[id])
				callbacks[id] = [];
			callbacks[id].push(f);
			return f;
		},

		execute : function(id) {
			while (callbacks[id] && callbacks[id].length > 0)
				callbacks[id].shift()(); // call listener and shift it from
											// array
		},

		reset : function() {
			callbacks = {};
		}
	};
}();

WGA.ajax.buildClientCallbackInformation = function(actionDef) {

	return {
		callback : actionDef.callback,
		params : actionDef.params,
		keepParams : actionDef.keepParams,
		encodeParams : actionDef.encodeParams,
		portletEvent : actionDef.portletEvent
	};

};

/**
 * /* Call ajax action /*
 * 
 * @param {Object}
 *		actionDef - see de.innovationgate.wgpublisher.webtml.utils.AjaxActionDefinition
 *			.action 			actionlink
 *			.id					portletId
 *			.graydiv 			grayout portlet (true/false)
 *			.callback	 		custom callback function only executed outside an tmlform
 *			.mode	 			valid values "norefresh"
 *			.tmlformSessionKey	sessionkey of server side tmlform (used during formcallback)
 *			.params 			Object containing URL params for the request 
 *			.keepParams 		Whether the URL params of the initial request should be kept. Defaults to true.
 *			.portletEvent		Objekt holding information about the portlet event that triggered the action, if any
 *			.portletEvent.id	ID of the portlet evnet
 *			.portletEvent.name	Name of the portlet evnet
 * @returns {void}
 */
/**/
WGA.ajax.action = function(actionDef) {
	
	var link = WGA.parseActionLink(actionDef.action);
	if ((actionDef.id == undefined || actionDef.id == null)
			&& link.portletKey != null) {
		actionDef.id = link.portletKey;
	}
	
	if (WGA.debug && console && console.log) {
		var msg = "";
		if (link.actionLink.charAt(0) == "$") {	
			msg+="Executing AJAX default action '" + link.actionLink + "'"
		}
		else {
			msg+="Executing AJAX action"
		}
		if (actionDef.portletEvent) {
			msg+=" triggered by portlet event '" + actionDef.portletEvent.name + "'";
		}
		msg+=", portlet: " + actionDef.id;
		if (link.formID) {
			msg+= ", form: " + link.formID;
		}
		console.log(msg);
	}
	
	// Fail early if the portlet div is not available
	var divTag = document.getElementById("$ajaxContentDiv_" + actionDef.id);
	if (!divTag) {
		return false;
	}

	WGA.ajax.runningPortlets[actionDef.id] = new Date();

	// Do normal post
	if (link.formID == null) {

		WGA.ajax.post(actionDef);
	}

	// Do AJAX form post. First transmit form data, the do post
	else {
 
		WGA.ajax.portletFormCallbacks[actionDef.id] = WGA.ajax
				.buildClientCallbackInformation(actionDef);
		var form = document.getElementById(link.formID);
		if (!form || !form.$formaction) {
			delete WGA.ajax.runningPortlets[actionDef.id];
			if(!form)
				alert("Error while calling action. TMLForm \"" + link.formID
						+ "\" not found.");
			else alert("TMLForm \"" + link.formID + "\" has no $formaction.");
			return false;
		}
		form.$formaction.value = actionDef.action;
		form.$ajaxcallid.value = actionDef.id;

		if (actionDef.graydiv != undefined) {
			form.$ajaxgraydiv.value = actionDef.graydiv;
		} else {
			form.$ajaxgraydiv.value = '#null#';
		}

		if (actionDef.mode) {
			form.$ajaxmode.value = actionDef.mode;
		} else {
			form.$ajaxmode.value = '#null';
		}

		var iframename = "$wga_ajaxIFrame_" + actionDef.id;
		var frame = document.getElementById(iframename);
		if (!frame) {
			frame = document.createElement('iframe');
			frame.id = iframename;
			frame.name = iframename;

			document.body.appendChild(frame);
			if (WGA.isIE)
				document.frames[iframename].name = iframename;
		}
		frame.style.position = "absolute";
		frame.style.visibility = "hidden";
		frame.style.left = -10000;
		frame.style.top = -10000;
		frame.style.width = frame.style.height = 0;

		form.target = iframename;
		form.action = WGA.contextpath + "/ajaxform";

		if (WGA.b4submit.onsubmit(form))
			form.submit();
	}
	
};

/**
 * Function called from iframe after a form is sumbittes via ajax
 * 
 * @param {Object}
 *            actionDef
 */
WGA.ajax.formCallback = function(actionDef) {

	var callbackInfo = WGA.ajax.portletFormCallbacks[actionDef.id];
	delete WGA.ajax.portletFormCallbacks[actionDef.id];

	actionDef.callback = callbackInfo.callback;
	actionDef.params = callbackInfo.params;
	actionDef.keepParams = callbackInfo.keepParams;
	actionDef.encodeParams = callbackInfo.encodeParams;
	actionDef.portletEvent = callbackInfo.portletEvent;

	WGA.ajax.post(actionDef);

	var iframename = "$wga_ajaxIFrame_" + actionDef.id;
	var frame = document.getElementById(iframename);
	if (frame) {
		document.body.removeChild(frame);
	}
};

/**
 * Old version of call ajax action DEPRECATED generate actionDef object and
 * calls new ajax.action()
 * 
 * @param {Object}
 *            action
 * @param {Object}
 *            id
 * @param {Object}
 *            graydiv
 * @param {Object}
 *            callback
 */
WGA.ajax.callAction = function(action, id, graydiv, callback, params) {
	WGA.ajax.action({
		action : action,
		id : id,
		graydiv : graydiv,
		callback : callback,
		params : params
	});
};

/**
 * Send ajax request
 * 
 * @param {String}
 *            method
 * @param {string}
 *            uri the uri to post the request to
 * @param {String}
 *            data the data to post
 * @param {Function}
 *            callback the function to be called on sucessfull answer from the
 *            server
 * @param {Object}
 *            header optional: request headers
 */
WGA.ajax.request = function(method, uri, data, callback, header) {

	var xmlHttpReq = false;
	// Mozilla/Safari
	if (window.XMLHttpRequest) {
		xmlHttpReq = new XMLHttpRequest();
	}
	// IE
	else if (window.ActiveXObject) {
		xmlHttpReq = new ActiveXObject("Microsoft.XMLHTTP");
	}

	if (!xmlHttpReq)
		return alert("unable to get xmlHttpRqeuest Object");

	xmlHttpReq.onreadystatechange = function() {
		if (xmlHttpReq.readyState == 4) {
			if (typeof (WGA) == "undefined")
				return; // page may be reloaded before callback was reached.

			callback(xmlHttpReq);

			// cleanup: so IE doesn't leak memory
			delete xmlHttpReq['onreadystatechange'];
			xmlHttpReq = null;
		}
	};

	var method = method.toUpperCase();
	xmlHttpReq.open(method, uri, true);
	if (header) {
		for (h in header)
			xmlHttpReq.setRequestHeader(h, header[h]);
	} else if (method == "POST")
		xmlHttpReq.setRequestHeader('Content-Type',
				'application/x-www-form-urlencoded');

	xmlHttpReq.send(data);
};

/**
 * Do the ajax call
 * 
 * @param {Object}
 *            actionDef
 */
WGA.ajax.post = function(actionDef, customObj) {

	var link = WGA.parseActionLink(actionDef.action);
	if ((actionDef.id == undefined || actionDef.id == null)
			&& link.portletKey != null) {
		actionDef.id = link.portletKey;
	}

	// retrieve ajaxInfo object
	var ajaxInfo = eval("$ajaxInfo_" + actionDef.id);

	// build url
	var strURL = location.pathname + "?$action=" + link.actionLink;

	if (customObj) {
		strURL = strURL + "&" + WGA.toQueryString(customObj, false);
	}

	/** event maintenance * */
	if (actionDef.mode != "norefresh") {
		// unregister all eventlistener of this portlet
		WGA.event.removeListeners(actionDef.id);
	}

	/** portlet registration maintenance * */
	// WGA.portlet.unregister(actionDef.id);
	// call registered b4post-listeners
	WGA.ajax.b4post.execute(actionDef.id);

	var divTag = document.getElementById("$ajaxContentDiv_" + actionDef.id);
	if (divTag != null) {
		if (actionDef.graydiv == undefined || actionDef.graydiv) {
			window.setTimeout(function(){
				WGA.util.maskElement(divTag);
			}, 250);
		}
	}
	else {
		delete WGA.ajax.runningPortlets[actionDef.id];
		return;
	}

	// build absolute URL for post - HTMLUnit tests do not support referencial
	// URLs
	var host = location.host;
	var protocol = location.protocol;
	var absoluteURL = protocol + "//" + host + strURL;

	// Build URL params
	var urlParams = (actionDef.params != undefined ? actionDef.params : {});
	urlParams["$ajaxInfo"] = ajaxInfo;
	if (actionDef.mode) {
		urlParams["$ajaxMode"] = actionDef.mode;
	}
	if (actionDef.tmlformSessionKey) {
		urlParams["$ajaxformkey"] = actionDef.tmlformSessionKey;
	}
	
	if (actionDef.portletEvent) {
		if (actionDef.portletEvent.index) {
			urlParams["$portletEvent"] = actionDef.portletEvent.index;
		}
		else {
			urlParams["$portletEventName"] = actionDef.portletEvent.name;
		}
		if (JSON) {
			urlParams["$portletEventParams"] = JSON.stringify(actionDef.portletEvent.params);
		}
	}

	var keepParams = (actionDef.keepParams == undefined ? WGA
			.isVersionComplianceSupporting(link.versionCompliance,
					"keepUrlParamsDefault") : actionDef.keepParams);
	var encodeParams = (actionDef.encodeParams == undefined ? WGA
			.isVersionComplianceSupporting(link.versionCompliance,
					"encodeUrlParamsDefault") : actionDef.encodeParams);

	var portletStates = [];
	WGA.portlet.collectPortletStates(portletStates, actionDef.id);
	if (portletStates.length > 0) {
		urlParams["$portletState"] = portletStates;
	}

	// do the request now:
	var queryString = WGA.toQueryString({}, keepParams, encodeParams, urlParams);
	if (queryString != "") {
		queryString = "&" + queryString;
	}
	var postData = WGA.toQueryString(urlParams, keepParams, encodeParams);
	WGA.ajax.request("POST", absoluteURL + queryString, postData, function(xmlHttpReq) {
		// callback after request
		delete WGA.ajax.runningPortlets[actionDef.id];

		if (actionDef.mode == 'norefresh') {
			WGA.ajax.callbackNoRefresh(xmlHttpReq);
		}
		else if (actionDef.callback == undefined) {
			WGA.ajax.replaceHTML(xmlHttpReq.responseText, actionDef.id); // default action if no userdefined callback is given
		}
		else {
			actionDef.callback(xmlHttpReq);
			WGA.portlet.onload.execute(actionDef.id);
			WGA.event.fireEvents();
		}
		// if robottests enabled - setFinishedFlag
		if (WGA.robotTest) {
			WGA.robotTest.idle();
		}
	});
};

/**
 * Replace div.innerHTML by string and ensure, that scripts are getting executed
 * 
 * @param {Object}
 *            str
 * @param {Object}
 *            id - id of the element to be replaced
 */
WGA.ajax.replaceHTML = function(str, id) {
	var divTag = document.getElementById("$ajaxDiv_" + id);
	if (divTag == null)
		return;
	// IE does not recognize the first script tag by getElementsByTagName
	// if there is no html:tag with content in front
	// so add a none displayed span-tag in front of the pasted code
	divTag.innerHTML = "<div id='$ajaxContentDiv_" + id
			+ "'><span style=\"display:none\">ajax</span>" + str + "</div>";

	// evaluate pasted javascript
	var totalscripts = "";
	var scripts = divTag.getElementsByTagName("script");
	for ( var i = 0; i < scripts.length; i++) {
		var script = scripts[i].innerHTML;
		totalscripts += "\ntry{" + script + "\n}catch(e){console && console.log('Script Error in AJAX content',e)}"
	}

	if (WGA.isIE) {
		WGA.ajax.executeScriptElement(totalscripts);
		WGA.portlet.onload.execute(id);
		WGA.event.fireEvents();
	} else {
		var scripts;
		do {
			scripts = divTag.getElementsByTagName("script");
			if (scripts.length) {
				var tag = scripts[0];
				tag.parentNode.removeChild(tag);
			} else
				break;
		} while (true)

		totalscripts += "\nWGA.portlet.onload.execute('" + id + "');";
		totalscripts += "\nWGA.event.fireEvents();";
		window.setTimeout(totalscripts, 10);
	}

};

/**
 * The following function is used when we are in IE only
 * 
 * @param {Object}
 *            script
 */
WGA.ajax.executeScriptElement = function(script) {
	var script_element = document.createElement("script");
	// first append as child of head
	// Opera execute scripts twice if the next to lines are changed in order
	// script_element.text = script;
	document.getElementsByTagName("head")[0].appendChild(script_element);
	script_element.text = script;
	document.getElementsByTagName("head")[0].removeChild(script_element);
};

/**
 * execute a ajax response as script The response should only contain one single
 * javascript text (like in JSON responds)
 * 
 * @param {Object}
 *            xmlHttpReq
 */
WGA.ajax.callbackNoRefresh = function(xmlHttpReq) {
	var script = xmlHttpReq.responseText;
	script += "\nWGA.event.fireEvents();";
	if (WGA.isIE)
		WGA.ajax.executeScriptElement(script);
	else
		window.setTimeout(script, 10);
};
/*
 * --- end AJAX Handling ---
 * 
 * 
 * /************************* event handling
 */
WGA.event = function() {

	var eventStack = [];
	var listeners = {};
	var timer;

	return {

		init: function(){
			eventStack = [];
			listeners = {};
			if (timer)
				window.clearTimeout(timer)
			timer=null;		
		},
		
		/**
		 * dispacht an event (put it on event stack) the event is not fired. use
		 * this function to dispacht multiple events and then call fireEvents()
		 * 
		 * @param {Object}
		 *            event
		 */
		dispatch : function(event) {

			if (WGA.debug && console && console.log) {
				console.log("Dispatching portlet event '" + event.name + "' from source '" + event.source + "', params: " + JSON.stringify(event.params));
			}
			
			eventStack.push(event);
		}

		/**
		 * dispacht an event (put it on event stack) and fires the event.
		 * 
		 * @param {Object}
		 *            event
		 */
		,
		fireEvent : function(eventname, source, params) {
			
			WGA.event.dispatch({
				name : eventname,
				source : source || "WGA",
				params : params || {}
			});
			WGA.event.fireEvents();
		}

		/**
		 * register an eventlistener for a portlet and event
		 * 
		 * @param {Object}
		 *            pKey - portlet key
		 * @param {Object}
		 *            eventName
		 * @param {Object}
		 *            f - function to be called back when event fires
		 */
		,
		addListener : function(pKey, eventName, f) {
			if (!listeners[pKey]) {
				listeners[pKey] = {};
			}
			if (!listeners[pKey][eventName])
				listeners[pKey][eventName] = [];
			listeners[pKey][eventName].push(f);
		}

		/**
		 * unregister all eventlistener of this portlet and all its child
		 * portlets
		 * 
		 * @param {Object}
		 *            pKey - portlet key
		 */
		,
		removeListeners : function(pkey) {
			delete listeners[pkey];
			// unregister all eventlistener of child portlets
			WGA.portlet.forEachChild(pkey, WGA.event.removeListeners);
		}

		/**
		 * fire all events (call event listeners) on event stack end delete
		 * event from stack
		 */
		,
		fireEvents : function() {
			if (WGA.ajax.hasRunningPortlets()) {
				if (timer)
					window.clearTimeout(timer)
				timer = window.setTimeout(WGA.event.fireEvents, 1000);
				return;
			}
			while (eventStack.length > 0) {
				var event = eventStack.shift();
				for ( var i in listeners) {
					if (listeners[i][event.name]) {
						var fns = listeners[i][event.name];
						for ( var j = 0; j < fns.length; j++) {
							try {
								
								if (WGA.debug && console && console.log) {
									console.log("Executing portlet event '" + event.name + "' for portlet " + i);
								}
								
								fns[j](event);
							} catch (e) {
								// catch JS-error in event listeners to ensure
								// ALL listeners will be called.
								WGA.util.showException(
										"Error in WGA event listener for event '"
												+ event.name + "'", e);
							}
						}
					}
				}
			}
		}

	};

}();

/**
 * If the page is loaded, call WGA.event.fireEvents()
 */
WGA.onload.register(WGA.event.fireEvents);
/* end event handling */

/**
 * portlet registry
 */
WGA.portlet = function() {

	// local variables:
	var childRegistry = {};
	var parentRegistry = {};
	var parentFormRegistry = {};
	var objectReg = {};
	var portletStates = {};
	var reloadPortlets = [];
	var PORTLETSTATE_PREFIX = "de.innovationgate.wga.portletstate.";
	var CHILDPORTLETS_PREFIX = "de.innovationgate.wga.childportlets.";
	
	function destroyObjects(portletkey) {
		var objects = objectReg[portletkey];
		if (objects) {
			for ( var i = 0; i < objects.length; i++) {
				var o = objects[i];
				if (o && o.destroy)
					o.destroy(portletkey);
				delete o;
			}
		}
		delete objectReg[portletkey];
	}

	return {

		init: function(){
			childRegistry = {};
			parentRegistry = {};
			parentFormRegistry = {};
			objectReg = {};
			portletStates = {};
			reloadPortlets = [];
		},
		
		/**
		 * WGA.portlet.onload registers function to be called when a portlet is
		 * refreshed by an ajax call Callbacks are not called on page loads like
		 * WGA.onload (why not ???) Because WGA.onload may have problems when
		 * many portlets are refreshed at the same time, WGA.portlet.onload is
		 * the better way to register callbacks
		 */
		onload : function() {

			var reg = {}; // local var to store callbacks

			return {

				/*
				 * register a function to be called when the portlet is loaded
				 * via ajax or a page refresh @param {Object} pkey - portlet key
				 */
				register : function(pkey, f) {
					if (pkey == "")
						return WGA.onload.register(f);
					if (!reg[pkey])
						reg[pkey] = [];
					reg[pkey].push(f);
					return f;
				},

				/**
				 * execute all registered onload-s for one portlet and delete it
				 * from registry will be called automaticaly on ajax-loads and
				 * should not be called directly the registered function will be
				 * called with the portlet key as parameter
				 * 
				 * @param {Object}
				 *            pkey - portlet key
				 */
				execute : function(pkey) {
					try {
						var fns = reg[pkey];
						while (fns && fns.length > 0)
							fns.shift()(pkey);
						// execute onload-s for all childportlets:
						WGA.portlet.forEachChild(pkey,
								WGA.portlet.onload.execute);
					} catch (e) {
						WGA.util.showException("portlet.onload exception", e);
					}
				},

				/**
				 * execute all registered onload-s for ALL portlets (and delete
				 * them from registry) will be called automaticaly on
				 * window.onload and should not be called directly
				 */
				executeAll : function() {
					for (pkey in reg) {
						WGA.portlet.onload.execute(pkey);
					}
				}
			};
		}() // end onload
		
		,getObjectReg : function() {
			return objectReg;
		}

		/*
		 * register portlet. @param {Object} pkey - portlet key
		 */
		,register : function(reg) {
			
			var pKey = reg.portletKey;
			
			// check if we have child portlets and destroy all registered
			// objects
			this.forEachChild(pKey, destroyObjects);
			destroyObjects(pKey);

			// add portlet as parent to child registry
			childRegistry[pKey] = {};
			for ( var i = 0; i < reg.parentKeys.length; i++) {
				var key = reg.parentKeys[i];
				childRegistry[key][pKey] = pKey;
			}
			// add portlet to parent registry
			parentRegistry[pKey] = reg.parentKeys;
			
			// Add parent form to registry
			parentFormRegistry[pKey] = reg.parentForm;
			
		}
		
		// Register state for a portlet
		// pKey: The portlet key
		// state: The serialized state
		// testBetterState: If a local state is available then prefer this to the given state
		// forceReload: Force reload of the portlet with the best state
		,registerState : function(pKey, state, processContextId, testBetterState, forceReload, defaultState) {
			
			var localIsBetter = false;
			
			/*
			 * #00004824:
			 * Test, if browser support sessionStorage
			 * Note that is't not enough to check window.sessionStorage
			 * Safari doesn't have local storage in private surfing mode. window.sessionStorage returns an Object
			 * but calling setItem() throws an exception "(DOM Exception 22): The quota has been exceeded".  
			 */
			function hasLocalStorage(){
				var testKey = 'test', storage = window.sessionStorage;
			    try {
			        storage.setItem(testKey, '1');
			        storage.removeItem(testKey);
			        return true;
			    } catch (error) {
			        return false;
			    }				
			}
			
			// Test if we have a local state.
			if (testBetterState) {
				var myState = this.fetchState(pKey);
				if (myState && myState.data != state) {
					localIsBetter = true;
					forceReload = true;
				}
			}
			
			if (!localIsBetter) {
				stateObject = JSON.stringify({
						data: state,
						defaultState: (defaultState && defaultState == true)
				});
				
				if (hasLocalStorage()) {
					window.sessionStorage.setItem(PORTLETSTATE_PREFIX +  WGA.uriHash + "." + pKey, stateObject);
					var parentKeys = parentRegistry[pKey];
					if (parentKeys) {
						
						var parentKey = parentKeys[0];
						if (parentKey) {
							var children = [];
							var childrenStr = window.sessionStorage.getItem(CHILDPORTLETS_PREFIX +  WGA.uriHash + "." + parentKey);
							if (childrenStr) {
								children = childrenStr.split(",");
							}
							
							var foundChild = false;
							for (var idx=0; idx < children.length; idx++) {
								var child = children[idx];
								if (child == pKey) {
									foundChild = true;
									break;
								}
							}
							
							if (!foundChild) {
								children.push(pKey);
							}
							window.sessionStorage.setItem(CHILDPORTLETS_PREFIX +  WGA.uriHash + "." + parentKey, children.join(","));
						}
					}
				}
				else {
					portletStates[pKey] = stateObject;
				}
			}
			
			if (forceReload) {
				this.registerPortletForReload(pKey, processContextId);
			}
		}
		
		,fetchState : function(pKey) {
			if (window.sessionStorage) {
				return JSON.parse(window.sessionStorage.getItem(PORTLETSTATE_PREFIX + WGA.uriHash + "." + pKey));
			}
			else {
				return JSON.parse(portletStates[pKey]);
			}
		}
		
		// Remove state of a portlet that has been explicitly unregistered
		,disposeState : function(pKey) {
			if (window.sessionStorage) {
				window.sessionStorage.removeItem(PORTLETSTATE_PREFIX + WGA.uriHash + "." + pKey);
			}
			else {
				delete portletStates[pKey];
			}
		}
		
		// Collect portlet states to be sent when the portlet of key pKey is reloaded
		,collectPortletStates : function(states, pKey) {
			
			// States of parent portlets
			var parentKeys = parentRegistry[pKey];
			for ( var i = 0; parentKeys && i < parentKeys.length; i++) {
				var parentKey = parentKeys[i];
				var state = this.fetchState(parentKey);
				if (state) {
					states.push(parentKey + "//" + state.data);
				}
			}
			
			// States of child portlets
			this.descendCollectPortletStates(states, pKey, true);
			
		}
		
		// Recursive inner functionality of collectPortletStates() to collect child portlets
		,descendCollectPortletStates : function(states, pKey, useDefaultStates) {

			var state = this.fetchState(pKey);
			if (state) {
				states.push(pKey + "//" + state.data);
			}
			
			if (window.sessionStorage) {
				var childrenStr = window.sessionStorage.getItem(CHILDPORTLETS_PREFIX + WGA.uriHash + "." + pKey);
				var children = [];
				if (childrenStr) {
					children = childrenStr.split(",");
				}
				for (var idx=0; idx < children.length; idx++) {
					WGA.portlet.descendCollectPortletStates(states, children[idx], true);
				}
			} 
			
			else {
				this.forEachChild(pKey, function(childKey) {
					WGA.portlet.descendCollectPortletStates(states, childKey, true);
				});
			}

		}
		
	
		// Registers a portlet to be reloaded at the end of the request
		,registerPortletForReload : function(pKey, processContextId) {
			if (reloadPortlets.length == 0 && !WGA.isPageLoaded) { // On Non-AJAX requests call performPortletReloads() on window load event. AJAX requests run this manually.
				WGA.onload.register(function() {
					WGA.portlet.performPortletReloads();
				});
			}
			reloadPortlets.push({key: pKey, processId: processContextId, children: []});
		}
		
		// Performs registered portlet reloads
		,performPortletReloads : function() {
			
			// Check every reload portlet if any of its parent should also be reloaded. If so, skip its reload as it will get anyway.
			var reallyReload = [];
			var processIds = [];
			var allPortletsToReload = {};
			for (var portletIdx=0; portletIdx < reloadPortlets.length; portletIdx++) {
				allPortletsToReload[reloadPortlets[portletIdx].key] = portletIdx;
			}
			
			for (var portletIdx=0; portletIdx < reloadPortlets.length; portletIdx++) {
				var reload = reloadPortlets[portletIdx];
				var parents = parentRegistry[reload.key];
				var anyParentContained = false;
				if (parents) {
					for (var parentIdx=0; parentIdx < parents.length; parentIdx++) {
						var parentKey = parents[parentIdx];
						var parentIdx = allPortletsToReload[parentKey];
						if (parentIdx) {
							reloadPortlets[parentIdx].children.push(reload);
							anyParentContained = true;
							break;
						}
					}
				}
				
				if (!anyParentContained) {
					reallyReload.push(reload);
				}
			}
			reloadPortlets = [];
			
			// Perform reloads
			for (var reallyReloadIdx=0; reallyReloadIdx < reallyReload.length; reallyReloadIdx++) {
				var reload = reallyReload[reallyReloadIdx];
				var formId = parentFormRegistry[reload.key];
				if (!formId) {
					formId = "##NULL##";
				}
				// Collect process ids
				var processIds = [];
				this.collectProcessIds(processIds, reload);
				
				if (WGA.debug && console && console.log) {
					console.log("Automatic portlet reload, portlet key: " + reload.key);
				}
				
				WGA.ajax.action({action: formId + "/" + reload.key + "/$refresh", params: {"$portletReloadOf": reload.key, "$reloadProcessContexts": processIds}, keepParams:true});
			}
		}
		
		// Recursively collect all process ids of portlets to reload
		,collectProcessIds : function(ids, reload) {
			ids.push(reload.processId);
			for (var childrenIdx=0; childrenIdx < reload.children.length; childrenIdx++) {
				this.collectProcessIds(ids, reload.children[childrenIdx]);
			}
		}
		
		// Generate RFC compliant unique ids.
		// A "best effort" approach generating random characters, plus letting the first chars be influenced by the time
		,generateUUID : function() {
		    var d = new Date().getTime();
		    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
		        var r = (d + Math.random()*16)%16 | 0;
		        d = Math.floor(d/16);
		        return (c=='x' ? r : (r&0x7|0x8)).toString(16);
		    });
		    return uuid;
		}

		/*
		 * unregister portlet. remove is from all parents and delete it from
		 * registries @param {Object} pkey - portlet key
		 */
		,unregister : function(pkey) {
			// remove this portlet as child from each parent
			var parentKeys = parentRegistry[pkey];
			for ( var i = 0; parentKeys && i < parentKeys.length; i++) {
				var parentKey = parentKeys[i];
				if (childRegistry[parentKey] && childRegistry[parentKey][pkey])
					delete childRegistry[parentKey][pkey];
			}
			delete childRegistry[pkey];
			delete parentRegistry[pkey];
		}

		/*
		 * utility function to call a user provided function for all
		 * childportlets the provided function will be called with the childrens
		 * portletkey as parameter @param {Object} pkey - portlet key @param
		 * {Object} f - the function to be called for each child portlet
		 */
		,forEachChild : function(pkey, f) {
			var childportlets = childRegistry[pkey];
			for ( var i in childportlets)
				f(childportlets[i]);
		}
		
		,registerObject : function(portletkey, obj) {
			if (!objectReg[portletkey])
				objectReg[portletkey] = [];
			objectReg[portletkey].push(obj);
			return obj;
		}
		
		,unregisterObject : function(portletkey, obj) {
			if (!objectReg[portletkey])
				return;
			
			var objects = objectReg[portletkey];
			for (var idx=0; idx < objects.length; idx++) {
				var regObj = objects[idx];
				if (regObj == obj) {
					var o = objects[idx];
					if (o.destroy)
						o.destroy(portletkey);
					delete o;
					return true;
				}
			}
			
			return false;
		}

	};
}();

// ensure portlet.onload-s will be called on window.onload as well:
WGA.onload.register(WGA.portlet.onload.executeAll);

/* end portlet registry */

WGA.websocket = {
		
		WINDOWID: "de.innovationgate.wga.windowId",
		
		reconnectAttempts: 1,
		
		reloadMessages: {
			"de": "Die aktuelle Seite muss neu geladen werden da die Web-Sitzung auf dem Server abgelaufen ist.<br>" +
					"Befinden sich ungesicherte Eingaben von ihnen auf dieser Webseite so werden sie diese eventuell nicht speichern können. Sichern sie diese also an einem anderen Ort um sie nicht zu verlieren.<br>" +
					"Klicken sie danach auf \"OK\" um neu zu laden oder laden sie die Seite manuell neu über die Reload-Schaltfläche ihres Browsers.",
			"en": "The current page needs to be reloaded because the web session on the server timed out.<br>" +
					"If the current webpage contains unsaved input from you then it is likely that it cannot be submitted. You should instead store it at another place to avoid losing it.<br>" + 
					"Then click \"OK\" to reload or perform the reload manually using the reload button of your browser."
		},
		
		backendLostMessages: {
			"de": "Die Websocket Verbindung zum Server wurde verloren. Eventuell ist ihre Internet-Verbindung abgebrochen oder der Serverdienst hat ein Problem.<br>" +
				"Befinden sich ungesicherte Eingaben von ihnen auf dieser Webseite so werden sie diese vermutlich nicht speichern können. Sichern sie diese also an einem anderen Ort um sie nicht zu verlieren.<br>" +
				"Klicken sie danach auf \"OK\" um zu versuchen, eine neue Verbindung aufzubauen oder laden sie die Seite manuell neu über die Reload-Schaltfläche ihres Browsers.",
			"en": "The Websocket connection to the server is gone. Maybe your internet connection is lost or the server service has problems.<br>" +
				"If the current webpage contains unsaved input from you then it is very likely that it cannot be submitted. You should instead store it at another place to avoid losing it.<br>" + 
				"Then click \"OK\" to try to build a new connection or perform the reload manually using the reload button of your browser."			
		},
		
		messageListeners: [],
		
		callbacks: {},
		
		errorCallbacks: {},
		
		start: function(url, pageId, sessionId, noCloseHandler) {
			this.url = url;
			this.pageId = pageId;
			this.sessionId = sessionId;
			this.noCloseHandler = noCloseHandler;
			if (!this.startService()) {
				this.noSupport();
			} 
		},
		
		startService: function() {
			
			if (!window.sessionStorage) {
				return false;
			}
			
			var windowId = window.sessionStorage.getItem(this.WINDOWID);
			var urlParams = {};
			if (!windowId) {
				windowId = this.pageId;
				window.sessionStorage.setItem(this.WINDOWID, windowId);
			}
			urlParams.pageId = this.pageId;
			urlParams.windowId = this.windowId;
			urlParams.sessionId = this.sessionId;
			
			var completeUrl = this.url + (this.url.indexOf("?") != -1 ? "&" : "?") + WGA.toQueryString(urlParams);
			

			if ('WebSocket' in window) {
				if (console && console.log) {
					console.log("Building socket connection, pageId: " + this.pageId);
				}
				this.socket = new WebSocket(completeUrl);
			}
			else if ('MozWebSocket' in window) {
				if (console && console.log) {
					console.log("Building mozilla compatible socket connection, pageId: " + this.pageId);
				}
				this.socket = new MozWebSocket(completeUrl);
			}
			else {
				return false;
			}
			
			this.socket.onmessage = this.onMessage;
			this.socket.onopen = this.onOpen;
			if (!this.noCloseHandler) {
				this.socket.onclose = this.onClose;
			}
			return true;
			
		},
		
		onMessage: function(event) {
			var msg = JSON.parse(event.data);
			if (msg.type == 'firePortletEvent') {
				if (console && console.log) {
					console.log("Execute portlet event from websocket, pageId: " + WGA.websocket.pageId, event.data);
				}	
				WGA.event.fireEvent(msg.event.name, "*", msg.event.params);
			}
			else if (msg.type == 'handshake') {
				WGA.websocket.reconnectAttempts = 1;
				if (console && console.log) {
					console.log("Connected WebSocket, pageId: " + WGA.websocket.pageId);
				}
			}
			else if (msg.type == 'response') {
				if (console && console.log) {
					console.log("Receiving response from websocket, pageId: " + WGA.websocket.pageId, event.data);
				}
				
				if (msg.status != "SUCCESS" && console && console.log) {
					console.log("WebSocket response returned with non-success status", JSON.stringify(msg));
				}
				if (!msg.callId) {
					return;
				}
				
				var callback;
				if (msg.status == "SUCCESS") {
					callback = WGA.websocket.callbacks[msg.callId];
					delete WGA.websocket.callbacks[msg.callId];
				}
				else {
					callback = WGA.websocket.errorCallbacks[msg.callId];
					delete WGA.websocket.errorCallbacks[msg.callId];			
				}
				
				if (callback && typeof(callback) == "function") {
					if (console && console.log) {
						console.log("Executing callback for response: " + callback);
					}
					callback(msg);
				}
			}
			else if (msg.type == 'custom') {
				for (var idx=0; idx < WGA.websocket.messageListeners.length; idx++) {
					if (console && console.log) {
						console.log("Execute custom message from websocket, pageId: " + WGA.websocket.pageId, event.data);
					}
					WGA.websocket.messageListeners[idx](msg.data);
				}
			}
		},

		onClose: function(event) {
			
			if (console && console.log) {
				console.log("Socket connection closed, pageId: " + WGA.websocket.pageId, "event.code", event.code);
			}
			
			WGA.websocket.socket.onmessage = null;
			WGA.websocket.socket.onopen = null;
			WGA.websocket.socket.onclose = null;
			
			// Reason codes that should prevent reconnect.
			if (event.code <= 1001) { // Normal closing
				return;
			}
			else if (event.code == -9825) {
				// MacOS: errSSLPeerBadCert - A bad certificate was encountered.
				// ignore this
				return;
			}
			else if (event.code == 1008) {
				if (console && console.log) {
					console.log("Lost connection to WebSocket bc. of violated policy. Will need to reload page to restart WebSocket", event.reason);
					WGA.util.showReloadMessage(WGA.util.label(WGA.websocket.reloadMessages, "en"));
				}
				return;
			}
			
			if (WGA.websocket.reconnectAttempts >= 5) {
				if (console && console.log) {
					console.log("Lost connection to WebSocket. Reason code: " + event.code + ", reason: '" + event.reason + "'. Cancelling service after 5 reconnect attempts.");
				}
				WGA.util.showReloadMessage(WGA.util.label(WGA.websocket.backendLostMessages, "en"));
				return;
			}
			
			var time = WGA.util.generateInterval(WGA.websocket.reconnectAttempts, 5000);
			if (console && console.log) {
				console.log("Lost connection to WebSocket. Reason code: " + event.code + ", reason: '" + event.reason + "'. Will try to reconnect in " + time + " milliseconds.");
			}
			setTimeout(function() {
				WGA.websocket.reconnectAttempts = WGA.websocket.reconnectAttempts + 1;
				if (console && console.log) {
					console.log("Trying reconnect to WebSocket. Attempt: " + WGA.websocket.reconnectAttempts);
				}
				WGA.websocket.startService();
			}, time);
		},
		
		
		callGlobal: function(global, method, params, callback, errorCallback) {
			
			var uid = WGA.portlet.generateUUID();
			if (callback) {
				WGA.websocket.callbacks[uid] = callback;
			}
			if (errorCallback) {
				WGA.websocket.errorCallbacks[uid] = errorCallback;
			}
			var msg = {
					type: "callGlobal",
					callId: uid,
					global: global,
					method: method,
					params: params
			};
			WGA.websocket.socket.send(JSON.stringify(msg));
		
		},
		
		// Overwrite this to run some functionality if there is no WebSocket support
		noSupport: function() {
		}
		
};

/*******************************************************************************
 * Stay compatible with older WGA versions:
 */
callAction = WGA.callAction;
callAjaxAction = WGA.ajax.callAction;
ajaxFormCallback = WGA.ajax.formCallback;

// deprecated:
WGA.event.register = WGA.event.addListener;

