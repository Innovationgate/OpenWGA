package de.innovationgate.webgate.api;

public interface WGContentChangeListener extends WGContentEventListener {

	/**
	 * Triggered just before a content is removed
	 * @param event Event information
	 * @return true, when the operation should continue. false if the removing should be canceled
	 */
	public boolean contentWillBeDeleted(WGContentEvent event) throws WGAPIException;
	
	/**
	 * Triggered when a struct entry has been updated
	 * @param event Event information
	 */
	public void structEntryHasBeenSaved(WGContentEvent event) throws WGAPIException;
	
}
