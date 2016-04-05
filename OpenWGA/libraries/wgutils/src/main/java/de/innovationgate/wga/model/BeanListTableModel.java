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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public abstract class BeanListTableModel<T> {
		private List<T> _beans;
		
		private Set<BeanListTableModelListener> _changeListeners = new HashSet<BeanListTableModelListener>();

		private String[] _columnIDs;
		
		private boolean _changed = false;
		
	      public BeanListTableModel() {
	            init();
	        }

		public BeanListTableModel(List<T> beans) {
			_beans = beans;
			init();
		}
		
		public void init() {
			List<String> ids = new ArrayList<String>();
			for (int i = 0; i < getColumnCount(); i++) {
				ids.add(new String("ID-" + i));
			}
			_columnIDs = ids.toArray(new String[0]);
		}
	 			
		public List<T> getBeans() {
			return _beans;
		}

		public abstract int getColumnCount();
		
		public abstract Object getColumnValue(T bean, int column);
		
		public abstract String getColumnText(T bean, int column);
		
		public abstract void setColumnValue(T bean, int column, Object value);
		
		public abstract boolean isColumnEditable(T bean, int column);
		
		public boolean isBeanEditable(T bean) {
			return true;
		}
		
		public boolean hideFromUI(T bean) {
			return false;
		}
		
		public void add(T bean) {
			_beans.add(bean);
			_changed = true;
			fireBeanAdded(bean);
		}

        protected void fireBeanAdded(T bean) {
            Iterator<BeanListTableModelListener> iterator = _changeListeners.iterator();
			while (iterator.hasNext()) {
				((BeanListTableModelListener) iterator.next()).add(bean);
			}
        }

		public void remove(T bean) {
			if (isBeanEditable(bean)) {
				_beans.remove(bean);
				_changed = true;
				fireBeanRemoved(bean);
			}
		}

        protected void fireBeanRemoved(T bean) {
            Iterator<BeanListTableModelListener> iterator = _changeListeners.iterator();
            while (iterator.hasNext()) {
            	iterator.next().remove(bean);
            }
        }

		public void update(T bean) {
			if (isBeanEditable(bean)) {
			    _changed = true;
				fireBeanUpdated(bean);
			}
		}

        protected void fireBeanUpdated(T bean) {
            Iterator<BeanListTableModelListener> iterator = _changeListeners.iterator();
            while (iterator.hasNext()) {
            	iterator.next().update(bean);
            }
        }
		
		public void refresh(List<T> beans) {
			_beans = beans;
			_changed = false;
			fireModelRefreshed(_beans);
		}

        public boolean isChanged() {
            return _changed;
        }

        protected void fireModelRefreshed(List<T> beans) {
            Iterator<BeanListTableModelListener> iterator = _changeListeners.iterator();
			while (iterator.hasNext()) {
				iterator.next().refresh(beans);
			}
        }

		public void removeListener(BeanListTableModelListener listener) {
			_changeListeners.remove(listener);
		}


		public void addListener(BeanListTableModelListener listener) {
			_changeListeners.add(listener);
		}

		public String[] getColumnIDs() {
			return _columnIDs;
		}	

	}
