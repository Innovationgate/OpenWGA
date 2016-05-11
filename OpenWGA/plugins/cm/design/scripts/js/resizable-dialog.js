BI.resizableDialog = function(){

	var dialog, submitButton, cancelButton;

	function initDialog() {
		dialog = BI.resizableDialog.dialog = new Ext.BasicDialog("BI-resizable-dialog", { 
	        modal:true,
			resizable: true,
			collapsible: false,
			expandable: true,
			shim: true,
			constraintoviewport: true,
	        width:500,
       		minHeight:440
		});
		
		dialog.addKeyListener(27, dialog.hide, dialog);
		
		submitButton = BI.resizableDialog.submitButton = dialog.addButton($L.save, function(){BI.resizableDialog.submit()}, dialog);
		cancelButton = BI.resizableDialog.cancelButton = dialog.addButton($L.cancel, dialog.hide, dialog);
		
		dialog.center();
	}
	
	function autoSize() {
		var contentwrapper = Ext.get("site-panel");
		var difference = Math.round(contentwrapper.getWidth() / 16);
		dialog.moveTo(contentwrapper.getX() + difference / 2, contentwrapper.getY() + difference / 2);	
		dialog.resizeTo(contentwrapper.getWidth() - difference, contentwrapper.getHeight() - difference);
	}
		
	function showButton(btn, txt, handler) {
		
		if (!btn.hidden && dialog.minWidth > 0) {
			dialog.minWidth -= btn.el.getWidth() + 20;
		}
		
		btn.setHandler(handler, dialog);
		btn.enable();
		btn.setText(txt);
		if (btn.hidden) {
			btn.show();
			if (btn.parentEl.isVisible()) {
				btn.parentEl.show();
			}
		}
		dialog.minWidth += btn.el.getWidth() + 20;
	}
	
	function hideButton(btn) {
		if (!btn.hidden) {
			dialog.minWidth -= btn.el.getWidth() + 20;
			btn.hide();
		}
	}
	
	function init(labels, el) {
		autoSize();
		var minWidth = 0;
		if (labels.submitButtonText) {
			submitButton.setText(labels.submitButtonText);
			submitButton.show();
			if (!submitButton.parentEl.isVisible()) {
				submitButton.parentEl.show();
			}
			submitButton.enable();
			minWidth += submitButton.el.getWidth() + 20;
		} else {
			submitButton.disable();
			submitButton.hide();
		}
		
		if (labels.cancelButtonText) {
			cancelButton.setText(labels.cancelButtonText);
		} else {
			cancelButton.setText($L.cancel);
		}
		minWidth += cancelButton.el.getWidth() + 20;
		cancelButton.enable();
		
		dialog.minWidth = minWidth;
		dialog.setTitle(labels.title);
		BI.resizableDialog.el=el;
	}
	
	return {
		initDialog : initDialog,
		init: init,
		showButton: showButton,
		hideButton: hideButton,
		autoSize: autoSize
	}
}();