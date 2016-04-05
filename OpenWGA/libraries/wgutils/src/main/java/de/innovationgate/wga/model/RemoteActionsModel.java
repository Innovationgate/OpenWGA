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

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.beans.csconfig.v1.RemoteAction;

public class RemoteActionsModel extends BeanListTableModel<RemoteAction> {

	public RemoteActionsModel(List<RemoteAction> beans) {
		super(beans);
	}

	@Override
	public int getColumnCount() {
		return 3;
	}

	@Override
	public String getColumnText(RemoteAction bean, int column) {
		switch (column) {
			case 0:
				return bean.getModuleName();	
			case 1:
				return WGADesignConfigurationModel.ACCESSLEVELS_REMOTE_ACTIONS.get(bean.getCallerLevel()).getValue();
			case 2:
				return WGUtils.serializeCollection(bean.getCallers(), ",");
		}
		return null;
	}

	@Override
	public Object getColumnValue(RemoteAction bean, int column) {
		switch (column) {
			case 0:
				return bean.getModuleName();	
			case 1:
				return bean.getCallerLevel();
			case 2:
				return WGUtils.serializeCollection(bean.getCallers(), ",");
		}
		return null;
	}

	@Override
	public boolean isColumnEditable(RemoteAction bean, int column) {
		return true;
	}

	@Override
	public void setColumnValue(RemoteAction bean, int column, Object value) {
		switch (column) {
		case 0:
			bean.setModuleName((String) value);
			break;
		case 1:
			bean.setCallerLevel((Integer) value);
			break;
		case 2:
			bean.setCallers(WGUtils.deserializeCollection((String)value, ","));
			break;
		}
		update(bean);
	}

}
