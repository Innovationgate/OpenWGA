require.config({
    paths: {

		"afw":							"/plugin-wga-app-framework/js",

    	// requirejs plugins:
		"css": 							"/plugin-wga-app-framework/js/amd:css",
		"async": 						"/plugin-wga-app-framework/js/amd:async",
		
		// AMD modules:
        "jquery": 						"/plugin-wga-app-framework/js/jquery-1.12.0.min",
        "bootstrap-js": 				"/plugin-wga-app-framework/file/bootstrap/all/js/bootstrap.min",
        "bootstrap-base": 				"/plugin-wga-app-framework/file/bootstrap/base/js/bootstrap.min",
        "jquery-ui": 					"/plugin-wga-app-framework/file/jquery-ui-1.11/build/jquery-ui.min",
        "jquery-ui-js": 				"/plugin-wga-app-framework/file/jquery-ui-1.11/source",
        "jquery-ui-css": 				"/plugin-wga-app-framework/file/jquery-ui-1.11/build/jquery-ui.min",
        
		// query plugins:        
        "jquery-columnslider": 			"/plugin-wga-app-framework/js/jquery-columnslider",        
        "jquery-input-placeholder": 	"/plugin-wga-app-framework/js/jquery-input-placeholder",
        "jquery-lightbox": 				"/plugin-wga-app-framework/js/jquery-lightbox",
        "jquery-lookup-surgestions": 	"/plugin-wga-app-framework/js/jquery-lookup-surgestions",
        "jquery-modal": 				"/plugin-wga-app-framework/js/jquery-modal",
        "jquery-scrollintoview": 		"/plugin-wga-app-framework/js/jquery-scrollintoview",
        "jquery-swipehandler": 			"/plugin-wga-app-framework/js/jquery-swipehandler",
        "jquery-tabhandler": 			"/plugin-wga-app-framework/js/jquery-tabhandler",
        "jquery-textarea-autogrow": 	"/plugin-wga-app-framework/js/jquery-textarea-autogrow",
        "jquery-tree": 					"/plugin-wga-app-framework/js/jquery-tree",
        "jquery-wga-drophandler": 		"/plugin-wga-app-framework/js/jquery-wga-drophandler",
        "jquery-slideshow": 			"/plugin-wga-app-framework/js/jquery-slideshow",
        "jquery-accordion": 			"/plugin-wga-app-framework/js/jquery-accordion"
        
	},
	
    shim: {
    	
    	"bootstrap-js": 				['jquery'],
    	"bootstrap-base": 				['jquery', 'css!/plugin-wga-app-framework/file/bootstrap/base/css/bootstrap.min.css?noext'],
    	"jquery-ui": 					["css!jquery-ui-css"],

		//"jquery-lightbox": 				["css!/plugin-wga-app-framework/stylesheet/jquery-lightbox.int.stylesheet?noext"],
		"jquery-lookup-surgestions": 	["css!/plugin-wga-app-framework/stylesheet/jquery-lookup-surgestions.int.stylesheet?noext"],
		//"jquery-modal": 				["css!/plugin-wga-app-framework/stylesheet/jquery-modal.int.stylesheet?noext"],
		"jquery-tree": 					["css!/plugin-wga-app-framework/stylesheet/jquery-tree.int.stylesheet?noext"]

    }
})
