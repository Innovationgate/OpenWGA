package de.innovationgate.wga.additional_script_langs.wcss;

public abstract class WcssResource {

	public abstract String getCode();
	public abstract WcssResource resolve(String path);
	
	public void addIntegratedResource(){}
	
	public String getCSSCode(){
		return getCode();	// overwrite when code processing is used.
	}

}
