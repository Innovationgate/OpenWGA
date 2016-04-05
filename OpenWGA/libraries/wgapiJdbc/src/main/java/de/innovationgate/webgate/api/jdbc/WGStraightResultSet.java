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

package de.innovationgate.webgate.api.jdbc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.Query;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGResultSetCore;

public abstract class WGStraightResultSet extends HibernateResultSet {

    public WGStraightResultSet(WGDatabaseImpl parent, Query query, Map<String,Object> queryParameters, Map<String,Object> queryOptions) {
        super(parent, query, queryParameters, queryOptions);
    }

    protected List wrapEntities(Iterator entities) throws WGAPIException {
    
        List wrapped = new ArrayList();
        Content content = null;
        while (entities.hasNext()) {
            content = null;
            Object entity = entities.next();
            
            Object wrappedEntity = wrapEntity(entity);
            
            if (wrappedEntity != null) {
                wrapped.add(wrappedEntity);
            }
        }
        return wrapped;
    
    }

    @Override
    protected Object wrapEntity(Object entity) throws WGAPIException {
        
        Object wrappedEntity = null;
        // Multi column output
        if (entity instanceof Object[]) {
            Object[] values = (Object[]) entity;
            if (values.length == 1 && (values[0] instanceof Content || values[0] instanceof ContentItem)) {
                wrappedEntity = wrapEntity(values[0]);
            }
            else {
                List resultList = wrapEntities(Arrays.asList((Object[]) entity).iterator());
                wrappedEntity = resultList.toArray();
            }
        }
        // Single column output
        else if (entity instanceof Content) {
            Content content = (Content) entity;
            wrappedEntity = getParent().createDocumentImpl(content);
        }
        else if (entity instanceof ContentItem) {
            Content content = ((ContentItem) entity).getParentcontent();
            wrappedEntity = wrapEntity(content);
        }
        else if (entity instanceof MainEntity) {
            wrappedEntity = getParent().createDocumentImpl((MainEntity) entity);
        }
        else {
            wrappedEntity = entity;
        }
        return wrappedEntity;
    }



}
