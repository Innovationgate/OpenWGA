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

import de.innovationgate.wga.common.beans.csconfig.v1.ACLRole;


public class ACLRolesModel extends BeanListTableModel<ACLRole> {

	public ACLRolesModel(List<ACLRole> beans) {
		super(beans);
	}

	@Override
	public int getColumnCount() {
		return 2;
	}

	@Override
	public String getColumnText(ACLRole bean, int column) {
		switch (column) {
			case 0:
				return bean.getName();	
			case 1:
				return Boolean.toString(bean.isDefaultManagerRole());
		}
		return null;
	}

	@Override
	public Object getColumnValue(ACLRole bean, int column) {
		switch (column) {
			case 0:
				return bean.getName();	
			case 1:
				return bean.isDefaultManagerRole();
		}
		return null;
	}

	@Override
	public boolean isColumnEditable(ACLRole bean, int column) {
		return true;
	}

	@Override
	public void setColumnValue(ACLRole bean, int column, Object value) {
		switch (column) {
		case 0:
			bean.setName((String) value);
			break;
		case 1:
			bean.setDefaultManagerRole((Boolean)value);
			break;
		}
		update(bean);
	}

}
