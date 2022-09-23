package de.innovationgate.wga.server.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;

public class WGAList extends ArrayList<Object>{

	private static final long serialVersionUID = 1L;

	public interface JSFunction {
        public Object call(Object a, Object b);
    }
		
	public WGAList(Collection<Object> list){
		super(list);
	}
	public WGAList(){
		super();
	}

	public String join(String divider){
		StringBuffer s = new StringBuffer();
		boolean firstLoop = true;
		for(Object o: this){
			if(!firstLoop)
				s.append(divider);
			s.append(o.toString());
			firstLoop = false;
		}
		return s.toString();
	}
	
	public WGAList map(JSFunction f){
		if(f==null)
			return this;
		ArrayList<Object> newList = new ArrayList<Object>();
		for(Object o: this){
			newList.add(f.call(o, null));
		}
		return new WGAList(newList);
	}

	public WGAList sortList(){
		Comparator<Object> compare = new Comparator<Object>() {
			@SuppressWarnings("unchecked")
			@Override
			public int compare(Object o1, Object o2) {
	            if (o1 instanceof Comparable && o2 instanceof Comparable) {
	            	if(o1 instanceof Integer)
	            		o1 = ((Integer) o1).doubleValue();
	            	if(o2 instanceof Integer)
	            		o2 = ((Integer) o2).doubleValue();
	                return ((Comparable<Object>) o1).compareTo(o2);
	            }
				return o1.toString().compareTo(o2.toString());
			}
		};
		sort(compare);
		return this;
	}

	public WGAList sortList(final JSFunction f){
		Comparator<Object> compare = new Comparator<Object>() {
			@Override
			public int compare(Object o1, Object o2) {
		        Object value = f.call(o1, o2);
		        if (value instanceof Number) {
		            return ((Number) value).intValue();
		        }
	            return 0;
			}
		};
		sort(compare);
		return this;
	}
	
	public WGAList trim(){
		ArrayList<Object> result = new ArrayList<Object>();
		for(Object o: this){
			if(o==null)
				continue;
			if(o instanceof String && ((String) o).trim().isEmpty())
				continue;
			result.add(o);
		}
		return new WGAList(result);
	}
	
    public WGAList deleteDoublets() {
        WGAList list = new WGAList();
        for(Object o: this){
        	if(!list.contains(o))
        		list.add(o);
        }
        return list;
    }

	public WGAList reverse(){
		Collections.reverse(this);
		return this;
	}

}
