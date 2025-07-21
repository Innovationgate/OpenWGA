package de.innovationgate.wga.server.api;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import de.innovationgate.utils.ObjectComparator;
import de.innovationgate.webgate.api.WGLanguage;

public class WGAList<T> extends ArrayList<T>{

	private static final long serialVersionUID = 1L;

	public interface JSFunction {
        public Object call(Object a, Object b);
    }
		
	public WGAList(Collection<? extends T> list){
		super(list);
	}
	public WGAList(){
		super();
	}

	public String join(String divider){
		StringBuffer s = new StringBuffer();
		boolean firstLoop = true;
		for(T o: this){
			if(!firstLoop)
				s.append(divider);
			s.append(o.toString());
			firstLoop = false;
		}
		return s.toString();
	}
	
	public WGAList<Object> map(JSFunction f){
		if(f==null)
			return (WGAList<Object>)this;
		WGAList<Object> newList = new WGAList<Object>();
		for(Object o: this){
			newList.add(f.call(o, null));
		}
		return newList.trim();
	}

	public WGAList<T> filter(JSFunction f){
		if(f==null)
			return this;
		WGAList<T> newList = new WGAList<T>();
		for(T o: this){
			if((boolean)f.call(o, null))
				newList.add(o);
		}
		return newList;
	}

	public WGAList<T> each(JSFunction f){
		if(f==null)
			return this;
		for(T o: this){
			f.call(o, null);
		}
		return this;
	}

	public WGAList<T> sortList(){
		Comparator<Object> comperator = new ObjectComparator();
		sort(comperator);
		return this;
	}
	public WGAList<T> sortList(String lang){
		Collator collator = Collator.getInstance(WGLanguage.languageNameToLocale(lang));
		ObjectComparator comperator = new ObjectComparator(collator);
		sort(comperator);
		return this;
	}

	public WGAList<T> sortList(final JSFunction f){
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
	
	public WGAList<T> trim(){
		WGAList<T> result = new WGAList<T>();
		for(T o: this){
			if(o==null)
				continue;
			if(o instanceof String && ((String) o).trim().isEmpty())
				continue;
			result.add(o);
		}
		return result;
	}
	
    public WGAList<T> deleteDoublets() {
        WGAList<T> list = new WGAList<T>();
        for(T o: this){
        	if(!list.contains(o))
        		list.add(o);
        }
        return list;
    }

	public WGAList<T> reverse(){
		Collections.reverse(this);
		return this;
	}	
	
	public WGAList<T> shuffle(){
		Collections.shuffle(this);
		return this;		
	}
	
	public int getLength(){
		return size();
	}
	
}
