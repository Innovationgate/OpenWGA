package de.innovationgate.webgate.api;

public interface PageAndAreaRightsFilter extends PageRightsFilter{

	public Right mayEditPages(WGArea area, WGUserAccess userAccess);
	
}
