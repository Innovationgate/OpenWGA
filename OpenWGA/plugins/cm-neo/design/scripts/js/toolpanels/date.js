define([
		"jquery", 
		"jquery-ui-datepicker"
	], function($){

	var editor;
	
	var el = $("#editor-panel-date")
	el.datepicker({
		changeMonth: true,
		changeYear: true,
		showButtonPanel: true,

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