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

package de.innovationgate.wgpublisher.problems;

import java.util.Locale;
import java.util.MissingResourceException;

import de.innovationgate.wga.modules.BundleLoader;
import de.innovationgate.wga.modules.LocalisationBundleLoader;

public class LocalizedProblemText implements ProblemText {
    
    private BundleLoader _loader;
    private String _messageKey;
    private String _baseKey;

    public LocalizedProblemText(BundleLoader loader, String key) {
        this(loader, key, null);
    }
    
    public LocalizedProblemText(BundleLoader loader, String baseKey, String messageKey) {
        _loader = loader;
        _baseKey = baseKey;
        _messageKey = baseKey + (messageKey != null ? "." + messageKey : "");
    }

    @Override
    public String getTitle(Locale l) {
        return _loader.getBundle(l).getString(_baseKey + ".title");
    }
    
    @Override
    public String getMessage(Locale l) {
        String key = _messageKey + ".message";
        try {
            
            return _loader.getBundle(l).getString(key);
        }
        catch (MissingResourceException e) {
            return key + " (Label not found)";
        }
    }

    @Override
    public String getDescription(Locale l) {
        String key = _messageKey + ".description";
        try {
            
            return _loader.getBundle(l).getString(key);
        }
        catch (MissingResourceException e) {
            return key + "(Label not found)";
        }
    }

    @Override
    public String getSolution(Locale l) {
        String key = _messageKey + ".solution";
        try {
            return _loader.getBundle(l).getString(key);
        }
        catch (MissingResourceException e) {
            return null;
        }
    }

}
