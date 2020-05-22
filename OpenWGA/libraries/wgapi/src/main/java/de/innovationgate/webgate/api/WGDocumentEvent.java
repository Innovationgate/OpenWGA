package de.innovationgate.webgate.api;

public class WGDocumentEvent {

	public static final int TYPE_SAVED = 10;
	public static final int TYPE_DELETED = 20;
	public static final int TYPE_MOVED = 30;
	private int _type;
	private WGDocumentKey _key;

	public WGDocumentEvent(int type, WGDocumentKey key){
		_type = type;
		_key = key;
	}

	public WGDocumentKey getDocumentKey(){
		return _key;
	}
	
	public int getDocType(){
		return _key.getDocType();
	}
	
	public int getType(){
		return _type;
	}
	public void setType(int type){
		_type=type;
	}
	
}
