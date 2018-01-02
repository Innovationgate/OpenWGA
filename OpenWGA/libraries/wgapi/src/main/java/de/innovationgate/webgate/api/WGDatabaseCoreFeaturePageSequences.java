package de.innovationgate.webgate.api;

public interface WGDatabaseCoreFeaturePageSequences {

	WGDocumentCore getStructEntryBySequence(long seq) throws WGAPIException;

	void createPageSequence(WGDocumentCore struct, boolean forceCreate) throws WGAPIException, InstantiationException, IllegalAccessException;
	
}
