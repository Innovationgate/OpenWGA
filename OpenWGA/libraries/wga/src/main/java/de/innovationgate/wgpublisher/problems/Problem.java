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

package de.innovationgate.wgpublisher.problems;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.modules.BundleLoader;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.mail.WGAMailNotification;

public class Problem extends WGAServerException implements ProblemQueueEvent {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static final String VAR_OPENER = "{";
    private static final String VAR_CLOSER = "}";
    
    public static class StaticMessageVariableReplaceProcessor implements ReplaceProcessor {
        
        private List<MessageVariableProvider> _variableProviders;
        
        public StaticMessageVariableReplaceProcessor(List<MessageVariableProvider> variableProviders) {
            _variableProviders = variableProviders;
        }
        
        @Override
        public int replace(String text, int from, int to, Writer out) throws IOException {

            int varCloser = text.indexOf(VAR_CLOSER, from);
            if (varCloser == -1) {
                out.write(VAR_OPENER);
                return from+1;
            }
            
            String variableName = text.substring(from + 1, varCloser).trim();
            Object var = getVariable(variableName, _variableProviders);
            if (var == null) {
                out.write("{unknown variable: " + variableName + "}");
                return varCloser + 1;
            }
            
            out.write(String.valueOf(var));
            return varCloser + 1;
            
        }
        
    }
    
    public static class Vars extends HashMap<String,Object> {
        
        /**
         * 
         */
        private static final long serialVersionUID = 1L;

        public Vars var(String var, Object value) {
            put(var, value);
            return this;
        }
        
    }
    
    public class MessageVariableReplaceProcessor implements ReplaceProcessor {
        
        private List<MessageVariableProvider> _variableProviders;
        
        public MessageVariableReplaceProcessor() {
        }
        
        @Override
        public int replace(String text, int from, int to, Writer out) throws IOException {
    
            int varCloser = text.indexOf(VAR_CLOSER, from);
            if (varCloser == -1) {
                out.write(VAR_OPENER);
                return from+1;
            }
            
            String variableName = text.substring(from + 1, varCloser).trim();
            Object var = getVariable(variableName);
            if (var == null) {
                out.write("{unknown variable: " + variableName + "}");
                return varCloser + 1;
            }
            
            out.write(String.valueOf(var));
            return varCloser + 1;
            
        }
        
    }

    public static Vars var(String var, Object value) {
        Vars vars = new Vars();
        return vars.var(var, value);
    }
    
    private ProblemText _text;
    private ProblemOccasion _occasion;
    private ProblemSeverity _severity;
    private Throwable _throwable;
    private Date _occuranceTime;

    private ProblemPath _path;
    private List<MessageVariableProvider> _variableProviders;
    public Problem(ProblemPath path, ProblemText text, ProblemSeverity severity, ProblemOccasion occasion, Throwable throwable, List<MessageVariableProvider> providers) {
        super(mergeVariables(text.getMessage(Locale.getDefault()), new StaticMessageVariableReplaceProcessor(providers)), throwable);
        _path = path;
        _text = text;
        _severity = severity;
        _occasion = occasion;
        _throwable = throwable;
        _occuranceTime = new Date();
        _variableProviders = providers;
    }
    
    @Override
    public String getLocalizedMessage() {
        return getMessage(Locale.getDefault());
    }
    
    public static Problem create(ProblemOccasion occasion, String key, ProblemSeverity severity) {
        return create(occasion, null, key, severity, null, null);
    }
    
    public static Problem create(ProblemOccasion occasion, String key, ProblemSeverity severity, Throwable throwable) {
        return create(occasion, null, key, severity, null, throwable);
    }
    
    public static Problem create(ProblemOccasion occasion, String key, ProblemSeverity severity, Vars vars, Throwable throwable) {
        return create(occasion, null, key, severity, vars, throwable);
    }
    
    public static Problem create(ProblemOccasion occasion, ProblemScope scope, String key, ProblemSeverity severity) {
        return create(occasion, scope, key, severity, null, null);
    }
    
    public static Problem create(ProblemOccasion occ, String key, ProblemSeverity severity, Vars vars) {
       return create(occ, null, key, severity, vars, null);
    }
    
    public static Problem create(ProblemOccasion occasion, ProblemScope scope, String key, ProblemSeverity severity, Throwable throwable) {
        return create(occasion, scope, key, severity, null, throwable);
    }
    
    public static Problem create(ProblemOccasion occ, ProblemScope scope, String key, ProblemSeverity severity, Vars vars) {
        return create(occ, scope, key, severity, vars, null);
    }

    
    public static Problem create(ProblemOccasion occasion, ProblemScope scope, String key, ProblemSeverity severity, final Vars vars, Throwable throwable) {

        BundleLoader loader;
        if (occasion instanceof UseSpecialTextLoader) {
            loader = ((UseSpecialTextLoader) occasion).createTextLoader(); 
        }
        else if (occasion.getDefaultRefClass() != null) {
            loader = new LocalisationBundleLoader(occasion.getDefaultRefClass(), "problems");
        }
        else {
            throw new IllegalArgumentException("Cannot determine resource to load problem text from");
        }
        
        ProblemText text;
        String baseKey = key;
        
        // Cutoff key trailer after "#" which is not used to build label keys
        int hashPos = baseKey.indexOf("#");
        if (hashPos != -1) {
            baseKey = baseKey.substring(0, hashPos);
        }
        
        // Determine type and scope
        ProblemType type;
        try {
            type = occasion.getDefaultType().newInstance();
        }
        catch (Exception e) {
            throw new IllegalStateException("Exception instantiating problem type", e);
        }
        
        if (scope == null) {
            scope =  occasion.getDefaultScope();
        }

        // Build base and message key for problem text: Try to extract message key after the first "."
        String messageKey = null;
        int pointPos = baseKey.lastIndexOf(".");
        if (pointPos != -1) {
            messageKey = baseKey.substring(pointPos + 1);
            baseKey = baseKey.substring(0, pointPos);
        }
        else if (occasion instanceof ProblemKeyQualificator) {
            messageKey = baseKey;
            baseKey = ((ProblemKeyQualificator) occasion).getBaseKey();
            
        }
        text = new LocalizedProblemText(loader, baseKey, messageKey);
        
        // Build problem text with provided variables
        List<MessageVariableProvider> providers = new ArrayList<MessageVariableProvider>();
        if (occasion instanceof MessageVariableProvider) {
            providers.add((MessageVariableProvider) occasion);
        }
        if (scope instanceof MessageVariableProvider) {
            providers.add((MessageVariableProvider) scope);
        }
        if (vars != null) {
            providers.add(new MessageVariableProvider() {
                @Override
                public Problem.Vars getDefaultMessageVariables() {
                    return vars;
                }
            });
        }
        
        // Build problem key. Qualify it with ref class name, if present
        String problemKey = key;
        if (occasion instanceof ProblemKeyQualificator) {
                problemKey = ((ProblemKeyQualificator) occasion).getBaseKey() + "." + problemKey;
        }
        else if (occasion.getDefaultRefClass() != null) {
                problemKey = occasion.getDefaultRefClass().getName() + "." + problemKey;
        }
        ProblemPath problemPath = new ProblemPath(occasion.getDefaultType(), scope, problemKey);
        
        Problem p;
        if (occasion instanceof UseSpecialProblemImplementation) {
            p = ((UseSpecialProblemImplementation) occasion).createProblem(problemPath, text, severity, occasion, throwable, providers);
        }
        else if (type instanceof UseSpecialProblemImplementation) {
            p = ((UseSpecialProblemImplementation) type).createProblem(problemPath, text, severity, occasion, throwable, providers);
        }
        else {
            p = new Problem(problemPath, text, severity, occasion, throwable, providers);
        }
        
        if(severity.equals(ProblemSeverity.HIGH)){
        	WGAMailNotification mail = new WGAMailNotification(WGAMailNotification.TYPE_PROBLEM);
        	mail.setSubject(p.getTitle(Locale.getDefault()));
        	mail.append(p.getMessage(Locale.getDefault()));
        	mail.append("<br>");
        	mail.append(p.getDescription(Locale.getDefault()));        	
			try {
				WGA.get().getCore().send(mail);
			} catch (WGException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }
        
        return p;
        
    }
    
    public String getMessage(Locale l) {
        return mergeVariables(_text.getMessage(l));
    }
    
    public String getTitle(Locale l) {
        return mergeVariables(_text.getTitle(l));
    }
    
    public String getDescription(Locale l) {
        return mergeVariables(_text.getDescription(l));
    }
    
    public String getSolution(Locale l) {
        return mergeVariables(_text.getSolution(l));
    }
    
    public Throwable getThrowable() {
        return _throwable;
    }
    
    
    public ProblemSeverity getSeverity() {
        return _severity;
    }

    public ProblemOccasion getOccasion() {
        return _occasion;
    }

    public ProblemPath getPath() {
        return _path;
    }

    public Date getOccuranceTime() {
        return _occuranceTime;
    }

    public static Object getVariable(String name, List<MessageVariableProvider> providers) {

        for (MessageVariableProvider provider : providers) {
            Vars vars = provider.getDefaultMessageVariables();
            if (vars != null) {
                Object value = vars.get(name);
                if (value != null) {
                    return value;
                }
            }
        }
        
        return null;
        
    }
    
    public Object getVariable(String name) {
        return getVariable(name, _variableProviders);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_path == null) ? 0 : _path.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Problem other = (Problem) obj;
        if (_path == null) {
            if (other._path != null)
                return false;
        }
        else if (!_path.equals(other._path))
            return false;
        return true;
    }
    
    private static String mergeVariables(String text, ReplaceProcessor proc) {
        
        if (text == null) {
            return null;
        }
        
        // If no var opener in label, we can bypass this operation
        if (text.indexOf(VAR_OPENER) == -1) {
            return text;
        }
        
        text = WGUtils.strReplace(text, VAR_OPENER, proc, true);
        
        return text;
        
    }
    
    private String mergeVariables(String text) {
        return mergeVariables(text, new MessageVariableReplaceProcessor());
    }




}
