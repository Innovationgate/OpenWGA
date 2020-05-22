package de.innovationgate.webgate.api;

public interface WGDocumentEventListener {

	public void handleEvent(WGDocumentEvent event) throws WGAPIException;
	
}
