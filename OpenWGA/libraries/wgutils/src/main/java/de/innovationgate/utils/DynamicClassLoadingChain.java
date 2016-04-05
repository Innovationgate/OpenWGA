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
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Class to maintain a chain of URLClassLoaders, that may dynamically grow and where individual loaders may be updated.
 * <p>
 * The DynamicClassLoadingChain builds a chain of URLClassLoaders for the purpose of retrieving classes from all loaders while keeping individual loaders separate
 * for addition and updating.
 * The loaders registered are called "subloaders". New subloaders can be added at runtime, where each new classloader has the
 * most recently added classloader as it's parent. So using the most recently added classloader - the "top loader" retrievable by {@link #getTopLoader()} 
 * - you can load all classes provided by all class loaders in the chain.
 * </p>
 * <p>
 * Also at runtime individual class loaders in the chain may be replaced. At adding time you can provide a key with each new class loader that you can
 * use later to address the loader to be replaced. Use method {@link #updateSubLoader(String, URL[])} for this. Replacing a class loader will implicitly
 * recreate all class loaders down the child chain, but leave all class loaders up the parent chain untouched.
 * </p>
 * <p>
 * When adding a loader you can declare it "static" (when using {@link #updateSubLoader(String, URL[], boolean)} and setting the last param to true). These loaders will be
 * added to the chain in a way that will prevent them from being updated and rebuilt. They are inserted to the chain before all non-static loaders and updating or
 * removing them will cause an {@link IllegalStateException} to be thrown. You can use static loaders for classes that need to be static in the classpath for their
 * functionalities to work. For example if you have a singleton class that maintains state, rebuilding the loader of the class will drop the singleton instance and by
 * the next use of it a new one will be created. By setting the loader to static you prevent such problems.
 * </p>
 */
public class DynamicClassLoadingChain extends ClassLoader {
    
    public class SubLoader extends URLClassLoader {
        
        private URL[] _originalUrls;
        private String _key;
        private SubLoader _childLoader = null;
        private boolean _static;
        
        public SubLoader(String key, URL[] urls, ClassLoader parent, boolean isstatic) {
            super(urls, parent);
            _originalUrls = urls;
            _key = key;
            _static = isstatic;
            _subLoaders.put(key, this);
            if (parent instanceof SubLoader) {
                ((SubLoader) parent).setChildLoader(this);
            }
        }


        public URL[] getOriginalUrls() {
            return _originalUrls;
        }

        public String getKey() {
            return _key;
        }


        public SubLoader getChildLoader() {
            return _childLoader;
        }


        public void setChildLoader(SubLoader childLoader) {
            _childLoader = childLoader;
        }


        public SubLoader recreate() {
            return recreateWithNewParent(getParent());
        }
        
        public SubLoader recreateWithNewParent(ClassLoader newParent) {
            SubLoader subLoader = new SubLoader(_key, _originalUrls, newParent, isStatic());
            subLoader.setChildLoader(getChildLoader());
            return subLoader;
        }


        public boolean isStatic() {
            return _static;
        }


        @Override
        public String toString() {
            return getClass().getName() + " [" + _key + ", " + _originalUrls.length + " libs" + (_static ? ", static" : "") + "]";
        }
        
    }

    private ClassLoader _originalParent;
    private SubLoader _bottomLoader = null;

    /**
     * Create a new chain.
     * @param parent The parent class loader to be used by the first subloader registered
     */
    public DynamicClassLoadingChain (ClassLoader parent) {
        super(parent);
        _topLoader = parent;
        _originalParent = parent;
    }
    
    private Map<String,SubLoader> _subLoaders = new ConcurrentHashMap<String, SubLoader>();
    private ClassLoader _topLoader = null;






    
    /**
     * Creates or updates a subloader for the given key.
     * @param key The of the loader
     * @param urls URLs to JARs or Java class folders that should be loaderd by this subloader
     * @param isstatic True if the loader should be static, i.e. should never be updated
     * @throws IllegalStateException When trying to update a subloader that is static
     */
    public synchronized void updateSubLoader(String key, URL[] urls, boolean isstatic) throws IllegalStateException {

        // Find the subloader to update
        SubLoader oldSubLoader = _subLoaders.get(key);
        SubLoader subLoader;
        
        // New loader
        if (oldSubLoader == null) {
            
            // Is a new dynamic subloader. We can just add it to the top of the chain
            if (!isstatic) {
                subLoader = new SubLoader(key, urls, _topLoader, isstatic);
                if (_bottomLoader == null) {
                    _bottomLoader = subLoader;
                }
            }
            else {
                subLoader = insertStaticLoaderToChain(key, urls);                
            }
        }
        
        // An existing subloader is just replaced with new data
        else {
            if (oldSubLoader.isStatic()) {
                throw new IllegalStateException("Subloader " + key + " is static and cannot be updated");
            }
            
            subLoader = new SubLoader(key, urls, oldSubLoader.getParent(), oldSubLoader.isStatic());
            subLoader.setChildLoader(oldSubLoader.getChildLoader());
            subLoader = rebuildChildChain(subLoader);
        }

        _topLoader = subLoader;
        
    }
    
    /**
     * Creates or updates a subloader for the given key.
     * This operation always creates non-static subloaders
     * @param key The of the loader
     * @param urls URLs to JARs or Java class folders that should be loaderd by this subloader
     */
    public synchronized void updateSubLoader(String key, URL[] urls) {
        updateSubLoader(key, urls, false);
    }
    
    /**
     * Retuns the url array registered for the subloader with the given key, in identical order as given to {@link #updateSubLoader(String, URL[])}
     * @param key The key of the subloader to use
     * @return The URLs of the subloader or null if no subloader of this name exists
     */
    public URL[] getSubLoaderURLs(String key) {
        
        SubLoader loader = _subLoaders.get(key);
        if (loader != null) {
            return loader.getOriginalUrls();
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Removes the subloader with the given key from the chain.
     * This operation implictly rebuilts all child class loaders of the removed one
     * @param key The key of the loader to remove
     * @throws IllegalStateException When trying to update a subloader that is static
     */
    public synchronized void removeSubLoader(String key) {
        
        SubLoader toRemove = _subLoaders.get(key);
        if (toRemove == null) {
            return;
        }
        
        if (toRemove.isStatic()) {
            throw new IllegalStateException("The loader is static and cannot be removed");
        }
        _subLoaders.remove(key);
        
        // Rebuild the child chain against the parent of the subloader to remove
        SubLoader oldChildLoader = toRemove.getChildLoader();
        ClassLoader parentOfRemovedOne = toRemove.getParent();
        if (oldChildLoader != null) {
            SubLoader subLoader = oldChildLoader.recreateWithNewParent(parentOfRemovedOne);
            subLoader.setChildLoader(oldChildLoader.getChildLoader());
            _topLoader = rebuildChildChain(subLoader);
        }
        else {
            if (parentOfRemovedOne instanceof SubLoader) {
                ((SubLoader) parentOfRemovedOne).setChildLoader(null);
            }
            
            _topLoader = parentOfRemovedOne;
        }
    
    }

    /**
     * Returns the topmost subloader of the chain, which can be used to load classes from all currently registered subloaders
     */
    public ClassLoader getTopLoader() {
        return _topLoader;
    }
    
    private synchronized SubLoader insertStaticLoaderToChain(String key, URL[] urls) {
        
        // Find the parent, which is the latest inserted static loader
        ClassLoader latestStaticLoader = getTopLoader();
        while (latestStaticLoader instanceof SubLoader && !((SubLoader) latestStaticLoader).isStatic()) {
            latestStaticLoader = latestStaticLoader.getParent();
        }
        
        // In case we found a static loader, we insert the new loader below it
        if (latestStaticLoader instanceof SubLoader && ((SubLoader) latestStaticLoader).isStatic()) {
            
            SubLoader latestStaticSubloader = (SubLoader) latestStaticLoader;
            SubLoader childLoader = latestStaticSubloader.getChildLoader();

            // Create the new static loader
            SubLoader staticLoader = new SubLoader(key, urls, latestStaticLoader, true);
            
            // Rebuild the child loader to point to the new static loader
            if (childLoader != null) {
                childLoader = childLoader.recreateWithNewParent(staticLoader);
                return rebuildChildChain(childLoader);
            }
            else {
                return staticLoader;
            }
        }
        
        // Else we must recreate the whole chain to position the new static loader to the top
        else {
            SubLoader oldBottomLoader = _bottomLoader;
            SubLoader staticLoader = new SubLoader(key, urls, _originalParent, true);
            _bottomLoader = staticLoader;
            if (oldBottomLoader != null) {
                SubLoader childLoader = oldBottomLoader.recreateWithNewParent(staticLoader);
                return rebuildChildChain(childLoader);
            }
            else {
                return staticLoader;
            }
        }
                
        
    }

    private SubLoader rebuildChildChain(SubLoader subLoader) {
        while (subLoader.getChildLoader() != null) {
            SubLoader childLoader = subLoader.getChildLoader();
            childLoader = (SubLoader) childLoader.recreateWithNewParent(subLoader);
            subLoader = childLoader;
        }
        return subLoader;
    }
    
    /**
     * Returns the keys of the classpath chain in the order of their loading priority (from parents to children) 
     */
    public List<String> getChainKeys() {
        
        List<String> keys = new ArrayList<String>();
        SubLoader subLoader = _bottomLoader;
        while (subLoader != null) {
            keys.add(subLoader.getKey());
            subLoader = subLoader.getChildLoader();
        }
        return keys;
        
    }

    public void clearAssertionStatus() {
        _topLoader.clearAssertionStatus();
    }

    public boolean equals(Object obj) {
        return _topLoader.equals(obj);
    }



    public URL getResource(String arg0) {
        return _topLoader.getResource(arg0);
    }

    public InputStream getResourceAsStream(String arg0) {
        return _topLoader.getResourceAsStream(arg0);
    }

    public Enumeration<URL> getResources(String arg0) throws IOException {
        return _topLoader.getResources(arg0);
    }

    public int hashCode() {
        return _topLoader.hashCode();
    }

    public Class<?> loadClass(String arg0) throws ClassNotFoundException {
        return _topLoader.loadClass(arg0);
    }

    public void setClassAssertionStatus(String arg0, boolean arg1) {
        _topLoader.setClassAssertionStatus(arg0, arg1);
    }

    public void setDefaultAssertionStatus(boolean arg0) {
        _topLoader.setDefaultAssertionStatus(arg0);
    }

    public void setPackageAssertionStatus(String arg0, boolean arg1) {
        _topLoader.setPackageAssertionStatus(arg0, arg1);
    }

    public String toString() {
        
        List<String> loaders = new ArrayList<String>();
        ClassLoader loader = _topLoader;
        while (loader instanceof SubLoader) {
            loaders.add(loader.toString());
            loader = loader.getParent();
        }
        return WGUtils.serializeCollection(loaders, "\n");
    }
    
    public boolean hasSubLoader(String key) {
        return _subLoaders.containsKey(key);
    }
    
    public SubLoader getSubLoader(String key) {
        return _subLoaders.get(key);
    }
    
    

}
