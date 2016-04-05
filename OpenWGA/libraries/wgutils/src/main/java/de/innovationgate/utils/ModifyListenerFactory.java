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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Factory to create maps, lists and objects which can be monitored
 * for modification by an {@link ModifyListener}
 */
public class ModifyListenerFactory implements InvocationHandler {
    
    private Object _target;
    private ModifyListener _reciever;
    private HashSet _modifyMethods = new HashSet();
    
    /**
     * creates an monitorable Map
     * ModifyLister.hasBeenModified() is invoked on the following map methods:
     * put, remove, clear
     * @param target The Map to monitor	
     * @param reciever The ModifyListener to notify
     */
    public static Map create( Map target, ModifyListener reciever) {
        return (Map)Proxy.newProxyInstance( target.getClass().getClassLoader(), target.getClass().getInterfaces(), 
                new ModifyListenerFactory(target, reciever));
    }
    
    /**
     * creates an monitorable List
     * ModifyLister.hasBeenModified() is invoked on the following list methods:
     * add, remove, removeAll, retainAll, clear
     * @param target The List to monitor
     * @param reciever The ModifyListener to notify
     */
    public static List create( List target, ModifyListener reciever) {
        return (List)Proxy.newProxyInstance( target.getClass().getClassLoader(), target.getClass().getInterfaces(), 
                new ModifyListenerFactory(target, reciever));
    }    
    
    /**
     * creates an monitorable Set
     * ModifyLister.hasBeenModified() is invoked on all data modification methods of Set
     * @param target The Set to monitor
     * @param reciever The ModifyListener to notify
     */
    public static Set create( Set target, ModifyListener reciever) {
        return (Set)Proxy.newProxyInstance( target.getClass().getClassLoader(), target.getClass().getInterfaces(), 
                new ModifyListenerFactory(target, reciever));
    }       
    
    /**
     * creates an monitorable Object
     * the object is wrapped by an {@link ModifyListenerObjectWrapper}
     * ModifyLister.hasBeenModified() is invoked on the method 'set()' of ModifyListenerObjectWrapper
     * @param target The Object to monitor
     * @param reciever The ModifyListener to notify
     */
    public static ModifyListenerObjectWrapper create( Object target, ModifyListener reciever) {
        ModifyListenerObjectWrapperImpl wrapper = new ModifyListenerObjectWrapperImpl(target);
        return (ModifyListenerObjectWrapper)Proxy.newProxyInstance( wrapper.getClass().getClassLoader(), wrapper.getClass().getInterfaces(), 
                new ModifyListenerFactory(wrapper, reciever));
    }
    
    private ModifyListenerFactory(ModifyListenerObjectWrapper target, ModifyListener reciever) {
        _target = target;
        _reciever = reciever;
        _modifyMethods.add("set");
    }
    
    private ModifyListenerFactory(Map target, ModifyListener reciever) {
        _target = target;
        _reciever = reciever;        
        _modifyMethods.add("put");
        _modifyMethods.add("putAll");
        _modifyMethods.add("remove");
        _modifyMethods.add("clear");
    }
    
    private ModifyListenerFactory(List target, ModifyListener reciever) {
        _target = target;
        _reciever = reciever;
        _modifyMethods.add("add");
        _modifyMethods.add("addAll");
        _modifyMethods.add("remove");
        _modifyMethods.add("removeAll");
        _modifyMethods.add("retainAll");
        _modifyMethods.add("clear");
    }
    
    private ModifyListenerFactory(Set target, ModifyListener reciever) {
        _target = target;
        _reciever = reciever;
        _modifyMethods.add("add");
        _modifyMethods.add("addAll");
        _modifyMethods.add("remove");
        _modifyMethods.add("removeAll");
        _modifyMethods.add("retainAll");
        _modifyMethods.add("clear");
    }
    
     
    /**
     * (non-Javadoc)
     * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    public Object invoke( Object proxy, Method method, Object[] args) throws Throwable {
        Object result = null;
        try {            
            result = method.invoke(_target, args);
            String name = method.getName();
            if (_modifyMethods.contains(name)) {
                _reciever.hasBeenModified();
            }
        } catch (InvocationTargetException e) {            
            throw e.getCause();
        }
        return result;
    }

}
