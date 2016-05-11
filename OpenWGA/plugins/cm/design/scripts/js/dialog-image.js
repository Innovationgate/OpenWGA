BI.resizableDialog.image = function(image, containerEl, otherDialogContents) {
	var imgcontainer = containerEl;
	
	var otherDialogContents = otherDialogContents || {
		width	:	0,
		height	:	0
	};
		
	function init() {
		image.ratio = image.startWidth / image.startHeight; 
		scaleToDialog(BI.resizableDialog.dialog.el.getWidth(), BI.resizableDialog.dialog.el.getHeight());
		BI.resizableDialog.dialog.fireEvent("afterinitialize", null);
	}
	
	function scaleToDialog(dialogWidth, dialogHeight) {
		if (dialogWidth && dialogHeight) {
			dialogWidth = dialogWidth - 30 - otherDialogContents.width;
			dialogHeight = dialogHeight - 85;
			var containerWidth = imgcontainer.getWidth();
			var containerHeight = imgcontainer.getHeight();
			
			if (containerWidth < dialogWidth) {
				containerWidth = dialogWidth;
				containerHeight = containerWidth / image.ratio;
			} else if (containerHeight < dialogHeight) {
				containerHeight = dialogHeight;
				containerWidth = containerHeight * image.ratio;
			}
			
			if (containerWidth > dialogWidth) {
				containerWidth = dialogWidth;
				containerHeight = containerWidth / image.ratio;
			}
			
			if (containerHeight > dialogHeight) {
				containerHeight = dialogHeight;
				containerWidth = containerHeight * image.ratio;;
			}
			
			imgcontainer.setX(Math.ceil(imgcontainer.getX()));
			imgcontainer.setY(Math.ceil(imgcontainer.getY()));
			
			imgcontainer.setWidth(containerWidth);
			imgcontainer.setHeight(containerHeight);
			scaleImage();
			BI.resizableDialog.dialog.fireEvent("dialogscaled");	
		}
	}
				
	function scaleImage() {	
		if (image.startHeight != imgcontainer.getHeight() || image.startWidth != imgcontainer.getWidth()) {
			if (image.startHeight > image.startWidth) {
				image.scalefactor = Math.round((imgcontainer.getHeight() / image.startHeight) * 100) / 100;
				
				image.img.setHeight(Math.round(image.startHeight * image.scalefactor));
				
				imgcontainer.setWidth(Math.round(image.startWidth * image.scalefactor));
				imgcontainer.setHeight(image.img.getHeight());
			} else {
				image.scalefactor = Math.round((imgcontainer.getWidth() / image.startWidth) * 100) / 100;
				image.img.setWidth(Math.round(image.startWidth * image.scalefactor));
				
				imgcontainer.setWidth(image.img.getWidth());
				imgcontainer.setHeight(Math.round(image.startHeight * image.scalefactor));
			}
		}
	}
		
	if (BI.resizableDialog.dialog.hasListener('resize')) {
		BI.resizableDialog.dialog.events.resize.clearListeners();
	}
	
	BI.resizableDialog.dialog.on('resize', function(t, width, height){
		if (imgcontainer) {
			scaleToDialog(width, height);				
		}
	});
	
	function addInitializeListener(t) {
		if (BI.resizableDialog.dialog.hasListener('afterinitialize')) {
			BI.resizableDialog.dialog.events.afterinitialize.clearListeners();
		}
		t.addListener('afterinitialize', function() {
			var newWidth = imgcontainer.getWidth() + 31 + otherDialogContents.width;
			if (newWidth < t.minWidth) {
				newWidth = t.minWidth;
			}
			
			var newHeight = imgcontainer.getHeight() + 86;
			if (newHeight < otherDialogContents.height + 86) {
				newHeight = otherDialogContents.height + 86;
			}
			
			t.resizeTo(newWidth, newHeight);
		}, this, {single: true});
		
	}
	
	if (BI.resizableDialog.dialog.hasListener('beforeshow')) {
		BI.resizableDialog.dialog.events.beforeshow.clearListeners();
	}	
	BI.resizableDialog.dialog.on('beforeshow', function(t) {
		//addInitializeListener(t);
		//init();
	}, this, {single: true});
	
	return {
		init : init,
		addInitializeListener: addInitializeListener
	};
};