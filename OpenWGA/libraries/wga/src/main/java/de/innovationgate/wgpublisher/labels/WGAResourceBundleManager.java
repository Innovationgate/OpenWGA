/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.wgpublisher.labels;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGDesignChangeEvent;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.wgpublisher.ManagedDBAttribute;

public class WGAResourceBundleManager implements WGDatabaseEventListener, ManagedDBAttribute, WGDesignChangeListener {



    private static final String SUFFIX_PROPERTIES = ".properties";

    private static final String NAME_DIVIDER = "_";

    public static final String CONTAINER_DEFAULT = "labels";

    public static final String FILE_DEFAULT = "general";

    private Map<String, Object> _cachedBundles = new HashMap<String, Object>();

    private WGDatabase _db;

    public class NoBundleIndicator {

    }

    public class LabelContainerName {

        private List<String> _elements;

        public LabelContainerName(List<String> elements) {
            _elements = elements;
        }

        public LabelContainerName(String name) {
            _elements = WGUtils.deserializeCollection(name, NAME_DIVIDER);
        }

        public LabelContainerName(String baseName, Locale locale) {

            _elements = new ArrayList<String>();
            _elements.add(baseName);

            if (locale != null) {
                _elements.add(locale.getLanguage());
                if (locale.getCountry() != null && !locale.getCountry().equals("")) {
                    _elements.add(locale.getCountry());
                }

                if (locale.getVariant() != null && !locale.getCountry().equals("")) {
                    _elements.add(locale.getVariant());
                }
            }
        }

        public LabelContainerName getParentName() {
            if (!hasParent()) {
                return null;
            }

            return new LabelContainerName(_elements.subList(0, _elements.size() - 1));
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        public String toString() {
            return WGUtils.serializeCollection(_elements, NAME_DIVIDER);
        }

        public boolean hasParent() {

            // Only has parent when there is still a language element left
            // The default container without language information is no parent
            // and is called separately
            return _elements.size() >= 3;
        }

        public int getLevel() {
            return _elements.size();
        }

    }

    public WGAResourceBundleManager(WGDatabase db) {
        _db = db;
        if (_db.getDesignProvider() != null) {
            db.getDesignProvider().addDesignChangeListener(this);
        }
        else {
            db.addDatabaseEventListener(this);
        }
    }

    public void databaseUpdate(WGDatabaseEvent event) {
        _cachedBundles.clear();
    }

    public boolean isTemporary() {
        return false;
    }

    public WGAResourceBundle getBundle(String container, String name, Locale locale) throws WGAPIException, IOException {
        return getBundle(new LabelContainerName(container, locale), name);
    }

    public WGAResourceBundle getBundle(LabelContainerName containerName, String bundleName) throws WGAPIException, IOException {

        // Get the container first so we can determine the up-to-date state
        WGFileContainer labelContainer = _db.getFileContainer(containerName.toString());
        if (labelContainer != null) {

            // First try to get from cache
            String cacheKey = containerName.toString() + NAME_DIVIDER + bundleName;
            Object cacheObject = _cachedBundles.get(cacheKey);

            // A bundle is in cache: return it if it is not stale
            if (cacheObject != null && cacheObject instanceof WGAResourceBundle) {
                WGAResourceBundle bundle = (WGAResourceBundle) cacheObject;
                if (bundle.getTime() >= labelContainer.getFileLastModified(getBundleFileName(bundleName)).getTime()) {
                    return bundle;
                }
            }

            // If there is no NoBundleIndicator in cache we try to retrieve the
            // bundle
            if (!(cacheObject instanceof NoBundleIndicator)) {

                // Try to find in container
                WGAResourceBundle bundle = getBundleFromContainer(labelContainer, bundleName);
                if (bundle != null) {
                    bundle.setLevel(containerName.getLevel());
                    if (containerName.hasParent()) {
                        bundle.setParent(getBundle(containerName.getParentName(), bundleName));
                    }
                    _cachedBundles.put(cacheKey, bundle);
                    return bundle;
                }

                // Put a NoBundleIndicator in cache to prevent further lookups
                // for this bundle which does not exist
                _cachedBundles.put(cacheKey, new NoBundleIndicator());
            }
            
        }

        // Request bundle from parent container
        if (containerName.hasParent()) {
            return getBundle(containerName.getParentName(), bundleName);
        }
        else {
            return null;
        }

    }

    private WGAResourceBundle getBundleFromContainer(WGFileContainer labelContainer, String bundleName) throws WGAPIException, IOException {
        String fileName = getBundleFileName(bundleName);
        if (labelContainer.hasFile(fileName)) {
            try (InputStream in = labelContainer.getFileData(fileName)) {
                WGAResourceBundle bundle = new WGAResourceBundle(in, labelContainer.getFileLastModified(fileName).getTime());
                return bundle;
            }
        }
        return null;
    }

    private String getBundleFileName(String bundleName) {
        return bundleName + SUFFIX_PROPERTIES;
    }

    public WGAResourceBundle getDefaultBundle(String containerName, String fileName) throws WGAPIException, IOException {

        return getBundle(new LabelContainerName(containerName, null), fileName);

    }

    public void close() {

        if (_db.getDesignProvider() != null) {
            _db.getDesignProvider().removeDesignChangeListener(this);
        }
        else {
            _db.removeDatabaseEventListener(this);
        }
        _db = null;
        _cachedBundles.clear();
    }

    public void designChanged(WGDesignChangeEvent event) {
        _cachedBundles.clear();
    }

    public WGDatabase getDb() {
        return _db;
    }

}
