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
package de.innovationgate.wga.model;

import de.innovationgate.wga.common.beans.csconfig.v2.Shortcut;

public class ShortcutType extends KeyValueBean<Integer, String> {
	
	public static final int TYPE_ITEM_MAPPING = Shortcut.TYPE_ITEM_MAPPING;
	public static final int TYPE_META_MAPPING = Shortcut.TYPE_META_MAPPING;
	public static final int TYPE_PLUGIN = Shortcut.TYPE_PLUGIN;
	public static final int TYPE_TMLSCRIPT_GLOBAL = Shortcut.TYPE_TMLSCRIPT_GLOBAL;

	public ShortcutType(Integer key, String value) {
		super(key, value);	
	}

}
