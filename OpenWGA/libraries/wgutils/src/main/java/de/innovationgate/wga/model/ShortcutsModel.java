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

import java.util.List;

import de.innovationgate.wga.common.beans.csconfig.v2.Shortcut;

public class ShortcutsModel extends BeanListTableModel<Shortcut> {

	public ShortcutsModel(List<Shortcut> beans) {
		super(beans);
	}

	@Override
	public int getColumnCount() {
		return 3;
	}

	@Override
	public String getColumnText(Shortcut bean, int column) {
		switch (column) {
			case 0:
				return bean.getShortcut();
			case 1:
				return WGADesignConfigurationModel.SHORTCUTTYPES.get(bean.getType()).getValue();
			case 2:
				return bean.getReference();				
		}
		return null;
	}

	@Override
	public Object getColumnValue(Shortcut bean, int column) {
		switch (column) {
			case 0:
				return bean.getShortcut();
			case 1:
				return bean.getType();
			case 2:
				return bean.getReference();				
		}
		return null;
	}

	@Override
	public boolean isColumnEditable(Shortcut bean, int column) {
		return true;
	}

	@Override
	public void setColumnValue(Shortcut bean, int column, Object value) {
		switch (column) {
			case 0:
				bean.setShortcut((String)value);
				break;
			case 1:
				bean.setType((Integer)value);
				break;
			case 2:
				bean.setReference((String)value);
				break;
		}		
		update(bean);
	}

}
