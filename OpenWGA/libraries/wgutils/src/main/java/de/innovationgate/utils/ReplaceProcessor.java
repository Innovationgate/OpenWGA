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

import java.io.IOException;
import java.io.Writer;

/**
 * A processor for replace operations done by {@link WGUtils#strReplace(String, String, ReplaceProcessor, boolean)}
 */
	public interface ReplaceProcessor {
		
		/**
         * Processes an replacement after a source string part was found that should be replaced
		 * @param text The complete source text
		 * @param from Start position for replacement
		 * @param to End position for replacement
		 * @param out Writer where the replacement should be written to
		 * @return The source text position from which to continue parsing
		 * @throws IOException
		 */
		public int replace(String text, int from, int to, Writer out) throws IOException;
		
	}
