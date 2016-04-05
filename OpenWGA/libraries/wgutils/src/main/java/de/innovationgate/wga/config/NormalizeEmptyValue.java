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
package de.innovationgate.wga.config;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * field will be normalized if value is empty
 * for e.g. 
 * - a String will be checked against its trimmed value, if the result is equals '' the field will be set to 'null'
 * - 'null' values from maps and collections will be removed, empty strings will NOT !!! 
 *
 */

@Constraint
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface NormalizeEmptyValue {
}
