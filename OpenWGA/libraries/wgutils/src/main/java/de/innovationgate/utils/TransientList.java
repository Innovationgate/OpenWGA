/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * wrapper for a list to use with session serialization
 * the original list is a transient field and is therefore not serialized
 * if the object is deserialized it behaves like an empty list
 *
 */
public class TransientList implements List, Serializable {
    
    private static final long serialVersionUID = -7731201565444180740L;
    
    private transient List _list;
    
    public TransientList(List originalList) {
        _list = originalList;
    }
    
    private void writeObject(ObjectOutputStream out) throws IOException {
        out.defaultWriteObject();
    }
    
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
         // "pseudo-constructor"
         in.defaultReadObject();
         // create empty list
         _list = new ArrayList();    
    } 

    public int size() {
        return _list.size();
    }

    public boolean isEmpty() {
        return _list.isEmpty();
    }

    public boolean contains(Object arg0) {
        return _list.contains(arg0);
    }

    public Iterator iterator() {
        return _list.iterator();
    }

    public Object[] toArray() {
        return _list.toArray();
    }

    public Object[] toArray(Object[] arg0) {
        return _list.toArray(arg0);
    }

    public boolean add(Object arg0) {
        return _list.add(arg0);
    }

    public boolean remove(Object arg0) {
        return _list.remove(arg0);
    }

    public boolean containsAll(Collection arg0) {
        return _list.containsAll(arg0);
    }

    public boolean addAll(Collection arg0) {
        return _list.addAll(arg0);
    }

    public boolean addAll(int arg0, Collection arg1) {
        return _list.addAll(arg0, arg1);
    }

    public boolean removeAll(Collection arg0) {
        return _list.removeAll(arg0);
    }

    public boolean retainAll(Collection arg0) {
        return _list.retainAll(arg0);
    }

    public void clear() {
        _list.clear();
    }

    public Object get(int arg0) {
        return _list.get(arg0);
    }

    public Object set(int arg0, Object arg1) {
        return _list.set(arg0, arg1);
    }

    public void add(int arg0, Object arg1) {
        _list.add(arg0, arg1);
    }

    public Object remove(int arg0) {
        return _list.remove(arg0);
    }

    public int indexOf(Object arg0) {
        return _list.indexOf(arg0);
    }

    public int lastIndexOf(Object arg0) {
        return _list.lastIndexOf(arg0);
    }

    public ListIterator listIterator() {
        return _list.listIterator();
    }

    public ListIterator listIterator(int arg0) {
        return _list.listIterator(arg0);
    }

    public List subList(int arg0, int arg1) {
        return _list.subList(arg0, arg1);
    }

}
