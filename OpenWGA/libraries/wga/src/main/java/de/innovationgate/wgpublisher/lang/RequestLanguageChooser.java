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

package de.innovationgate.wgpublisher.lang;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class RequestLanguageChooser implements WGLanguageChooser {
    
    private LanguageBehaviour _langBehaviour;
    private HttpServletRequest _req;

    public RequestLanguageChooser(WGDatabase db, HttpServletRequest req) {
        _langBehaviour = LanguageBehaviourTools.retrieve(db);
        _req = req;
    }

    public WGContent selectContentForName(WGDatabase db, String name, boolean isBI) throws WGAPIException {
        return _langBehaviour.requestSelectContentForName(db, _req, name, isBI);
    }

    public WGContent selectContentForPage(WGStructEntry page, boolean isBI) throws WGAPIException {
        return _langBehaviour.requestSelectContentForPage(page, _req, isBI);
    }

    public WGLanguage selectDatabaseLanguage(WGDatabase db) throws WGAPIException {
        return _langBehaviour.requestSelectDatabaseLanguage(db, _req);
    }

    public WGContentKey chooseQueryContent(WGDatabase db, Map<String, WGContentKey> contents) throws WGAPIException {
        throw new WGNotSupportedException("RequestLanguageChooser cannot do this");
    }

    public List<WGLanguage> getQueryLanguages(WGDatabase db) throws WGAPIException {
        throw new WGNotSupportedException("RequestLanguageChooser cannot do this");
    }
    
    @Override
    public List<WGContent> selectContentPriorityOrder(WGStructEntry entry, boolean isBI) throws WGAPIException {

        if (_langBehaviour instanceof PriorizingLanguageBehaviour) {
            return ((PriorizingLanguageBehaviour) _langBehaviour).requestSelectPriorityOrder(entry, _req, isBI);
        }
        else {
            return LanguageBehaviourTools.defaultSelectPriorityOrder(entry, _req, isBI);
        }
    
    }

}
