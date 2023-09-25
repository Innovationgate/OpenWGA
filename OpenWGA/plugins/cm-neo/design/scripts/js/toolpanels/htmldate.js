define([
		"jquery", 
		"jquery-ui-js/datepicker", 
		"jquery-ui-js/i18n/datepicker-de",
		"css!jquery-ui-css"
	], function($){

	var editor;
	
	var el = $("#editor-panel-htmldate")
	el.datepicker({
		changeMonth: true,
		changeYear: true,
		showButtonPanel: true,
		dateFormat: "yy-mm-dd",

		onSelect: function(date, ui){
			editor.setContent(date);
		}
	})
	
	return {
		setEditor: function(editor_obj){
			editor = editor_obj
			el.datepicker("setDate", editor.getContent());
		}
	}
	
})