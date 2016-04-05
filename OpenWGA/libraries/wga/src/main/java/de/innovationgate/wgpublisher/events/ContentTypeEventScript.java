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
package de.innovationgate.wgpublisher.events;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class ContentTypeEventScript implements EventReceiver {
    
    public static final Pattern EVENTSCRIPT_DEFINITION = Pattern.compile("^([a-z]+)/(.*)$", Pattern.DOTALL);
		
	private String _type;
	private String _code;
    private String _description;
    
    public ContentTypeEventScript(String scriptDefinition, String description) {
        
        // Try to match the definition with definition pattern
        Matcher matcher = EVENTSCRIPT_DEFINITION.matcher(scriptDefinition);
        if (matcher.matches()) {
            _type = matcher.group(1);
            _code = matcher.group(2);
        }
        
        // If the pattern does not match we take the whole definition as TMLScript event script
        else {
            _type = "tmlscript";
            _code = scriptDefinition;
        }
                
        _description = description;
        
    }
		
		
	/**
	 * @return
	 */
	public String getCode() {
		return _code;
	}

	/**
	 * @return
	 */
	public String getType() {
		return _type;
	}

    public String getDescription() {
        return _description;
    }
    
    /**
     * @param eventScript
     * @param event
     * @return
     * @throws WGExpressionException 
     */
    public List<Object> execute(EventManager eventManager, Event event) throws WGExpressionException {
        
        // Empty scripts might get stored by CM
        if (WGUtils.isEmpty(getCode())) {
            return null;
        }
        
        // We will only process ContentTypeEvents
        if (!(event instanceof ContentTypeEvent)) {
            return null;
        }
    
        // Get engine
        ExpressionEngine engine = ExpressionEngineFactory.getEngine(getType());
        if (engine == null) {
            EventManager.LOG.error("Error executing event script. Unknown script type: " + getType());
            return null;
        }
        
        // Create necessary context objects
        TMLContext context;
        try {
            ContentTypeEvent ctEvent = (ContentTypeEvent) event; 
            context = new TMLContext(ctEvent.getdocument(), eventManager.getCore(), ctEvent.getUserProfile(), null);
            TMLContext threadMainContext = TMLContext.getThreadMainContext();
            if (threadMainContext != null) {
                context.importEnvironmentData(threadMainContext, false);
            }
        }
        catch (Exception e) {
            EventManager.LOG.error("Error creating context for event script.", e);
            return null;
        }
        Map<String,Object> objects = new HashMap<String,Object>();
        objects.put("event", event);
        objects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, getDescription());

        // Execute script       
        ExpressionResult result = engine.evaluateExpression(getCode(), context, ExpressionEngine.TYPE_SCRIPT, objects);
        if (result.isError()) {
            throw result.getException();
        }
        else {
            return Collections.singletonList(result.getResult());
        }
        
    }

}
