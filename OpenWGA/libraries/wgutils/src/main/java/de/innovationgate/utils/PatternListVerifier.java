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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Class that takes a list of regular expression patterns and tests strings against all of them.
 * This class will cache result decisions.
 */
public class PatternListVerifier {

    private List _patterns = new ArrayList();
    private Map _cachedDecisions = WGUtils.createSynchronizedMap();

    public PatternListVerifier() {

        // Verify that the regex engine is available
        try {
            Class.forName("java.util.regex.Pattern");
        }
        catch (ClassNotFoundException e1) {
            throw new IllegalStateException("You need a Java VM of version 1.4 or higher to use this feature.");
        }
    }
    
    /**
     * Adds a regular expression pattern to the pattern list
     * @param patternStr The pattern as string
     */
    public void addPattern(String patternStr) {
        Pattern pattern = Pattern.compile(patternStr);
        _patterns.add(pattern);
        clearDecisionCache();
    }

    /**
     * Clears the cache of result decisions.
     */
    public void clearDecisionCache() {
        _cachedDecisions.clear();
    }

    /**
     * Verifies a string against all patterns in this verifier.
     * If any pattern matches it is returned. If none matches null is returned.
     * @param str The string to verify
     * @return A matching pattern or null
     */
    public Pattern verify(String str) {

        // Try to retrieve from decision cache
        if (_cachedDecisions.containsKey(str)) {
            return (Pattern) NullPlaceHolder.read(_cachedDecisions.get(str));
        }

        // Iterate thru patterns and test each
        Pattern matchingPattern = null;
        Iterator patterns = _patterns.iterator();
        Pattern pattern;
        while (patterns.hasNext()) {
            pattern = (Pattern) patterns.next();
            boolean matches = pattern.matcher(str).matches();
            if (matches == true) {
                matchingPattern = pattern;
                break;
            }
        }

        // Cache decision and return
        _cachedDecisions.put(str, NullPlaceHolder.write(matchingPattern));
        return matchingPattern;

    }

    /**
     * Returns the patterns on this verifier. The list in not modifiable.
     */
    public List getPatterns() {
        return Collections.unmodifiableList(_patterns);
    }

}
