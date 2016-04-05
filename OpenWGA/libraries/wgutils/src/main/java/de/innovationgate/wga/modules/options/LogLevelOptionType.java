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

import java.nio.charset.Charset;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;

/**
 * An option type storing log levels like those used by log4j
 *
 */
public class LogLevelOptionType extends PredefinedValuesOptionType {
    
    private static LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(LogLevelOptionType.class) + "/optiontypes", TextEncodingOptionType.class.getClassLoader());
    public static final LogLevelOptionType INSTANCE = new LogLevelOptionType();

    private LogLevelOptionType() {
        super(_bundleLoader, "loglevel");
        addValue("DEBUG");
        addValue("INFO");
        addValue("WARN");
        addValue("ERROR");
        addValue("FATAL");
    }
    
    
}
