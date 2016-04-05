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

import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;

public class PublisherOptionsModel extends BeanListTableModel<PublisherOption> {

	private WGADesignConfigurationModel _model;

    public PublisherOptionsModel(WGADesignConfigurationModel model) {
		super(model.getPublisherOptions());
		_model = model;
	}

	@Override
	public int getColumnCount() {
		return 2;
	}

	@Override
	public String getColumnText(PublisherOption bean, int column) {
		switch (column) {
			case 0:
				return bean.getName();	
			case 1:
				return bean.getValue();
		}
		return null;
	}

	@Override
	public Object getColumnValue(PublisherOption bean, int column) {
		return getColumnText(bean, column);
	}

	@Override
	public boolean isColumnEditable(PublisherOption bean, int column) {
		if (column == 0) {
			return false;
		} else {
			return true;
		}
	}

	@Override
	public void setColumnValue(PublisherOption bean, int column, Object value) {
		switch (column) {
		case 1:
			bean.setValue((String)value);
			break;
		}
		update(bean);
	}

	@Override
	public boolean isBeanEditable(PublisherOption bean) {
		if (_model.getDirectModifiablePublisherOptions().contains(bean.getName())) {
			return false;
		}
		return true;
	}

	@Override
	public boolean hideFromUI(PublisherOption bean) {
		return !isBeanEditable(bean);
	}
}
