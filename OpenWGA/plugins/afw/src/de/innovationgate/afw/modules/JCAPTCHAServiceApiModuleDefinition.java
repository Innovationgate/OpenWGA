/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.afw.modules;

import java.util.Locale;

import de.innovationgate.afw.modules.types.CAPTCHAServiceModuleType;
import de.innovationgate.igutils.security.CaptchaService;
import de.innovationgate.igutils.security.SimpleCaptchaService;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;

public class JCAPTCHAServiceApiModuleDefinition implements ModuleDefinition {

    public String getTitle(Locale locale) {
        return "JCAPTCHA Service";
    }

    public String getDescription(Locale locale) {
        return "A CAPTCHA service based on the JCaptcha library. This is deprecated as it does not work since Java 8.";
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Class<? extends ModuleType> getModuleType() {
        return CAPTCHAServiceModuleType.class;
    }

    public Class<? extends Object> getImplementationClass() {
        return CaptchaService.class;
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public Object getProperties() {
        return null;
    }

}
