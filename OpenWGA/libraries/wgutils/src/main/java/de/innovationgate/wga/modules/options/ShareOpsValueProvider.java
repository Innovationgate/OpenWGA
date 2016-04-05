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

package de.innovationgate.wga.modules.options;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;

/**
 * An option value provider offering operations to enable/disable for a content share: create, move, delete
 */
public class ShareOpsValueProvider extends LocalizedOptionValueProvider {
    
    public static final String DELETE = "delete";

    public static final String MOVE = "move";

    public static final String CREATE = "create";

    private static LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(ShareOpsValueProvider.class) + "/optiontypes", ShareOpsValueProvider.class.getClassLoader());
    
    public static final ShareOpsValueProvider INSTANCE = new ShareOpsValueProvider();

    private ShareOpsValueProvider() {
        super("sharerights", _bundleLoader);
        addValue(CREATE);
        addValue(MOVE);
        addValue(DELETE);
    }

}
