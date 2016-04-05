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

package de.innovationgate.wga.model;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

public interface Model {

    public abstract void addListener(ModelListener listener);

    public abstract void removeListener(ModelListener listener);

    public abstract void fireModelChanged();

    public abstract void saveChanges() throws IOException, IllegalAccessException, InvocationTargetException;

    public abstract void reload() throws IOException;

    public abstract List<ValidationError> validate();

    public abstract boolean isEditable(String propertyName);

    public abstract void restoreDefaults() throws IOException;

    public abstract boolean isListenerNotificationEnabled();

    public abstract void setListenerNotificationEnabled(boolean listenerNotificationEnabled);

}