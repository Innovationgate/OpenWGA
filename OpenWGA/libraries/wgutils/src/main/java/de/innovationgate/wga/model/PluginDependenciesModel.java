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

import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;

public class PluginDependenciesModel extends BeanListTableModel<PluginID> {


	public PluginDependenciesModel(List<PluginID> beans) {
		super(beans);
	}
	
	public int getColumnCount() {
		return 2;
	}

	@Override
	public Object getColumnValue(PluginID bean, int column) {
		switch (column) {
			case 0:				
				return bean.getUniqueName();				
			case 1:
				return bean.getVersion().getMainVersionString();
		}
		return null;
	}

	@Override
	public boolean isColumnEditable(PluginID bean, int column) {
		return true;
	}

	@Override
	public void setColumnValue(PluginID bean, int column, Object value) {
		switch (column) {
			case 0:				
				String sValue = (String) value;
				if (sValue != null) {
					sValue = sValue.trim();
				}
				bean.setUniqueName(sValue);	
				update(bean);
				break;
			case 1:
				try {				
					bean.setVersion(new Version((String)value));
					update(bean);
				} catch (RuntimeException e) {
					// unparsable version
				    throw new IllegalArgumentException("Unparseable plugin version");
				}
				break;
		}		
	}

	@Override
	public String getColumnText(PluginID bean, int column) {		
		switch (column) {
			case 0:				
				return bean.getUniqueName();				
			case 1:
				return bean.getVersion().toString();
		}
		return null;
	}
	


}
