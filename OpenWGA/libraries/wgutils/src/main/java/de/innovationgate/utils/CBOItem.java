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
package de.innovationgate.utils;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import javax.swing.JComboBox;

/**
 * A simple tool class to be used as item for Swing combo boxes.
 * At many occasions we want to select a value via combo box, that should be displayed
 * differently from the actual value. For that we build simple beans that return the display
 * value via toString(), so the combo box displays it, and return the value to store/process/whatever
 * on another method.
 * 
 * This now is a generic implementation for such a cbo item bean that differs between a title
 * (the display value) and the value (the actual value). It also implements equals() and
 * hashCode() based on the value. Some other nice tool methods for combo box handling are included.
 */
public class CBOItem {
    
    /**
     * Sorts a list of CBOItem objects by their title
     * @param items The items
     */
    public static void sortByTitle(List items) {
        
        Collections.sort(items, new Comparator() {
            public int compare(Object arg0, Object arg1) {
                CBOItem i0 = (CBOItem) arg0;
                CBOItem i1 = (CBOItem) arg1;
                return i0.getTitle().compareTo(i1.getTitle());
            }
            
        });
        
    }
    
    /**
     * Adds all CBOItem objects to the options of a combo box
     * @param items The items to add
     * @param box The combo box
     */
    public static void addAllItems(List items, JComboBox box) {
        Iterator itemsIt = items.iterator();
        while (itemsIt.hasNext()) {
            CBOItem item = (CBOItem) itemsIt.next();
            box.addItem(item);
            
        }
    }
    
    /**
     * Returns the selected value of a combo box, whose options
     * are represented by CBOItems.
     * Not the selected CBOItem object itself is returned but it's value.
     * @param box
     */
    public static Object getSelectedValue(JComboBox box) {
        CBOItem item = (CBOItem) box.getSelectedItem();
        return item.getValue();
    }
    
    /**
     * Sets the selected value of a combo box, whose options are represented by CBOItems.
     * @param box The combo box
     * @param value The selected value. Do not specify the CBOItem to select but its value.
     * @return true, if the value was found in the options and set. false if the value is not available as option.
     */
    public static boolean setSelectedValue(JComboBox box, Object value) {
        for (int idx=0; idx < box.getItemCount(); idx++) {
            CBOItem item = (CBOItem) box.getItemAt(idx);
            if (item.getValue().equals(value)) {
                box.setSelectedItem(item);
                return true;
            }
        }
        return false;
    }
    
    
    private String title;
    private Object value;
    
    /**
     * Constructs a CBOItem with a title and a value
     * @param title
     * @param value
     */
    public CBOItem(String title, Object value) {
        super();
        this.title = title;
        this.value = value;
    }
    
    /**
     * Returns the title
     */
    public String getTitle() {
        return title;
    }
    /**
     * Returns the value
     */
    public Object getValue() {
        return value;
    }

    public String toString() {
        return title;
    }

    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((value == null) ? 0 : value.hashCode());
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final CBOItem other = (CBOItem) obj;
        if (value == null) {
            if (other.value != null)
                return false;
        }
        else if (!value.equals(other.value))
            return false;
        return true;
    }

}
