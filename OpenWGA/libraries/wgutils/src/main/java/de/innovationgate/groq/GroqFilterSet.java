/*
 * Created on Dec 2, 2006 from ow
 *
 */
package de.innovationgate.groq;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.Format;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.apache.commons.jxpath.JXPathContext;

public abstract class GroqFilterSet<T> implements Iterator<T> {

    private Iterator<T> _it;
    private T _nextItem = null;
    private boolean _fetched = false;
    private boolean _endReached = false;
    private GroqSelectionSet<T> _set;

    @SuppressWarnings("unchecked")
    protected GroqFilterSet(GroqSelectionSet<T> set, Iterator<T> it) {
        
        if (set == null && this instanceof GroqSelectionSet) {
            set = (GroqSelectionSet<T>) this;
        }
        
        _set = set;
        _it = it;
    }
   
    public boolean hasNext() {
        
        fetchNextItem();
        return !_endReached;
        
    }

    private void fetchNextItem() {
        if (!_fetched && !_endReached) {
            _endReached = true;
            while (_it.hasNext()) {
                _nextItem = _it.next();
                if (survivesFilter(_nextItem)) {
                    _endReached = false;
                    break;
                }
            }
            
            _fetched = true;
        }
        
    }

    protected abstract boolean survivesFilter(T nextItem);

    public T next() {
        fetchNextItem();
        if (_endReached) {
            throw new NoSuchElementException("End reached");
        }
        else {
            _fetched = false;
            return _nextItem;
        }
    }
    
    /**
     * Returns the next element or null if no further element is available
     */
    public Object nextOrNull() {
        fetchNextItem();
        if (_endReached) {
            return null;
        }
        else {
            _fetched = false;
            return _nextItem;
        } 
    }


    
    public GroqFilterSet<T> whereEquals(Object value) {
        return new GroqEqualsFilterSet<T>(_set, this, value, false);
    }
    
    public GroqFilterSet<T> whereEqualsIgnoreCase(Object value) {
        return new GroqEqualsFilterSet<T>(_set, this, value, true);
    }
    
    public GroqFilterSet<T> wherePropertyEqualsRegexp(String prop, String regexp) {
        return new GroqPropertyEqualsRegexpSet<T>(_set, this, prop, regexp);
    }
    
    public GroqFilterSet<T> whereStartsWith(String start) {
        return new GroqStartsWithFilterSet<T>(_set, this, start);
    }
    
    public GroqFilterSet<T> whereEqualsRegexp(String regexp) {
        return new GroqEqualsRegexpFilterSet<T>(_set, this, regexp);
    }
    
    public GroqFilterSet<T> wherePropertyEquals(String prop, Object value) {
        return new GroqPropertyEqualsSet<T>(_set, this, prop, value);
    }
    
    public List<T> list() {
        
        List<T> list = new ArrayList<T>();
        while (hasNext()) {
            list.add(next());
        }
        return list;
        
    }
    
    public List<String> substring(int start, int length) {
        
        List<String> list = new ArrayList<String>();
        while (hasNext()) {
            T value = next();
            String strValue = String.valueOf(value);
            strValue = strValue.substring(start, length);
            list.add(strValue);
        }
        return list;
        
    }
    
    public List<String> substring(int start) {
        
        List<String> list = new ArrayList<String>();
        while (hasNext()) {
            T value = next();
            String strValue = String.valueOf(value);
            strValue = strValue.substring(start);
            list.add(strValue);
        }
        return list;
        
    }
    
    public List<Object> listProperty(String xpath, boolean includeNull) {
        List<Object> list = new ArrayList<Object>();
        while (hasNext()) {
            Object obj = next();
            JXPathContext cx = createJXPathContext(obj);
            Object propValue = cx.getValue(xpath);
            if (propValue != null || includeNull) {
                list.add(propValue);
            }
        }
        return list;
    }
    
    public List<Object> callMethod(String methName, Object... params) throws SecurityException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
        
        List<Object> list = new ArrayList<Object>();
        while (hasNext()) {
            Object obj = next();
            
            Method meth = obj.getClass().getMethod(methName, extractClasses(params));
            if (meth != null) {
                Object retVal = meth.invoke(obj, params);
                if (retVal != null) {
                    list.add(retVal);
                }
            }
        }
        return list;
        
    }
    
    public List<Object> listProperty(String xpath) {
        return listProperty(xpath, false);
    }

    public void remove() {
        throw new UnsupportedOperationException("This iterator is not modifiable");
    }
    
    /**
     * Removes the selected elements from the base collection
     */
    public void removeFromCollection() {
        
        List<T> elemsToRemove = list();
        _set.getCollection().removeAll(elemsToRemove);
        
    }
    
    public List<Object> constructObjects(Class clazz, String... paramXPath) throws SecurityException, NoSuchMethodException, IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException {
        List<Object> list = new ArrayList<Object>();
        while (hasNext()) {
            Object obj = next();
            Object[] params = retrieveParams(obj, paramXPath);
            Constructor con = clazz.getConstructor(extractClasses(params));
            if (con != null) {
                Object newObj = con.newInstance(params);
                list.add(newObj);
            }
        }
        return list;
    }

    private Class[] extractClasses(Object[] params) {
        return (Class[]) Groq.selectFrom(params).getClasses();
    }
    
    public Class[] getClasses() {
        
        List<Object> list = new ArrayList<Object>();
        while (hasNext()) {
            Object obj = next();
            list.add(obj.getClass());
        }
        return (Class[]) list.toArray(new Class[list.size()]);
    }

    private Object[] retrieveParams(Object obj, String[] paramXPath) {
        return Groq.selectFrom(paramXPath).useAsXPathOn(obj).toArray();
    }
    
    public List<Object> useAsXPathOn(Object obj) {
        
        List<Object> list = new ArrayList<Object>();
        while (hasNext()) {
            String xpath = next().toString();
            JXPathContext con = createJXPathContext(obj);
            Object value = con.getValue(xpath);
            list.add(value);
        }
        return list;
        
        
    }

    private JXPathContext createJXPathContext(Object obj) {
        JXPathContext con = JXPathContext.newContext(obj);
        con.setLenient(true);
        return con;
    }
    
    public List<Object> format(Format format) {
        
        List<Object> list = new ArrayList<Object>();
        while (hasNext()) {
            Object obj = next();
            String str = format.format(obj);
            list.add(str);
        }
        return list;
        
    }
    
    public void perform(Task task) {
        
        List<Object> list = new ArrayList<Object>();
        while (hasNext()) {
            Object obj = next();
            task.perform(obj);
        }
       
    }
    
    public Object singleValue() {
        if (hasNext()) {
            return next();
        }
        else {
            return null;
        }
    }

    protected GroqSelectionSet<T> getSet() {
        return _set;
    }

}
