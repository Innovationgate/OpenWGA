package de.innovationgate.webgate.api;

public interface WGContentChangeListener extends WGContentEventListener {

	/**
	 * Triggered just before a content is removed
	 * @param contentEvent Event information
	 * @return true, when the operation should continue. false if the removing should be canceled
	 */
	public boolean contentWillBeDeleted(WGContentEvent contentEvent) throws WGAPIException;

}
