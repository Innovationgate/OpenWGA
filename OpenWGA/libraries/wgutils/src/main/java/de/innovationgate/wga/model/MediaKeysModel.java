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

import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;

public class MediaKeysModel extends BeanListTableModel<MediaKey> {

	public MediaKeysModel(List<MediaKey> beans) {
		super(beans);
	}

	@Override
	public int getColumnCount() {
		return 4;
	}

	@Override
	public String getColumnText(MediaKey bean, int column) {
		switch (column) {
			case 0:
				return bean.getKey();	
			case 1:
				return bean.getMimeType();
			case 2:
				return Boolean.toString(bean.isBinary());
			case 3:
				return Boolean.toString(bean.isHttpLogin());
		}
		return null;
	}

	@Override
	public Object getColumnValue(MediaKey bean, int column) {
		switch (column) {
			case 0:
				return bean.getKey();	
			case 1:
				return bean.getMimeType();
			case 2:
				return bean.isBinary();
			case 3:
				return bean.isHttpLogin();
		}
		return null;
	}

	@Override
	public boolean isColumnEditable(MediaKey bean, int column) {
		return true;
	}

	@Override
	public void setColumnValue(MediaKey bean, int column, Object value) {
		switch (column) {
			case 0:
				bean.setKey((String)value);	
				break;
			case 1:
				bean.setMimeType((String)value);
				break;
			case 2:
				bean.setBinary((Boolean)value);
				break;
			case 3:
				bean.setHttpLogin((Boolean)value);
				break;
		}
		update(bean);
	}

}
