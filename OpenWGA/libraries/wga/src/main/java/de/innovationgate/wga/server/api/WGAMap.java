package de.innovationgate.wga.server.api;

import java.util.HashMap;
import java.util.Map;

public class WGAMap<K,V> extends HashMap<K,V>{

	private static final long serialVersionUID = 1L;

	public interface JSFunction {
        public Object call(Object a, Object b);
    }
	
	public WGAMap(Map<K, ? extends V> map){
		super(map);
	}
	
	public WGAMap(){
		super();
	}
	
	public WGAMap<K,V> each(JSFunction f){
		if(f==null)
			return this;
		for(Map.Entry<K, V> entry: this.entrySet()){
			f.call(entry.getKey(), entry.getValue());
		}
		return this;
	}

	public WGAMap<K,V> filter(JSFunction f){
		if(f==null)
			return this;
		WGAMap<K,V>  newMap = new WGAMap<K,V> ();
		for(Map.Entry<K, V> entry: this.entrySet()){
			if((boolean)f.call(entry.getKey(), entry.getValue()))
				newMap.put(entry.getKey(), entry.getValue());
		}
		return newMap;
	}
	
	public WGAList<Object> map(JSFunction f){
		if(f==null)
			return null;
		WGAList<Object> list = new WGAList<Object>();
		for(Map.Entry<K, V> entry: this.entrySet()){
			list.add(f.call(entry.getKey(), entry.getValue()));
		}
		return list.trim();
	}

}
