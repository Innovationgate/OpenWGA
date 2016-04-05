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
package de.innovationgate.wgpublisher.expressions.tmlscript;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.utils.WGUtils;

/**
 * Util class to parse variable arguments in TMLScript functions.
 * The VarArgParser gets a definition of arguments that can be available by using
 * the add() methods. The order in which argument definitions are added is the order
 * in which they will be expected on a method call.
 * 
 * A single call of #pack() must be done after all arguments were added so the VarArgParser can compile the possible arg combinations.
 * 
 * On a method call the method parse() should be called with
 * the arguments array as parameter. It returns an Arguments object that can be
 * used to read the arguments. 
 * 
 * The var arg parser allows schemes of mandatory/optional arguments according to the following rules:
 * 
 * <ul>
 * <li>The scheme definition may have a single block of mandatory arguments that may be preceded and followed by n optional arguments
 * <li>An optional argument on a function/method call may only be used if:
 *      <ul>
 *          <li>It is immediately followed/preceded by the mandatory block OR
 *          <li>If all optional arguments in the direction to the mandatory block are also used (i.e. none is omitted)
 *      </ul>
 * <li>
 * 
 * This prevents usages where an optional argument is omitted while a later optional argument is used, which is counter-intuitive and hard to judge.
 * 
 * Imagine the following argument definition:
 * 
 * someMethod(String arg1 (mandatory), String arg2 (optional), String arg3 (optional))
 * 
 * If this method was used with 2 string parameters then theoretically two interpretations of argument usage are possible:
 * 
 * someMethod(arg1, arg2)
 * someMethod(arg1, arg3)
 * 
 * But as the rule disallows "intermediate skipping" of optional parameters it can only be interpreted as
 * 
 * someMethod(arg1, arg2)
 * 
 * Which is what is commonly expected.
 *
 */
public class VarArgParser {
    
    private List<ArgDefinition> _arguments = new ArrayList<ArgDefinition>();
    
    private int _mandatoryBlockStart = -1;
    private int _mandatoryBlockEnd = -1;
    
    private boolean _unwrapOverflowArgs = true;
    private String _functionName;
    
    private boolean _compatibleMode = false;

    private List<List<ArgDefinition>> _combinations = null;
    
    
    /**
     * An object containing the arguments of a function call
     */
    public class Arguments {
        
        private Map<String,Object> _result;
        private List<Object> _overflowArgs;


        private Arguments(Map<String,Object> result, List<Object> overflowParams) {
            _result = result;
            _overflowArgs = overflowParams;
        }

        /**
         * Returns additional arguments of the call that were not defined
         * in the VarArgsParser
         */
        public List<Object> getOverflowArgs() {
            return _overflowArgs;
        }
        
        /**
         * Returns the argument of the given name
         * @param name The argument name
         * @return The argument value
         */
        public Object get(String name) {
            return _result.get(name);
        }
        
        /**
         * Tests if the argument of that name was provided
         * @param name The argument name
         * @return true if the argument was provided, false otherwise
         */
        public boolean has(String name) {
            return _result.containsKey(name);
        }
        
    }
    
    class ArgDefinition {
        
        private String _name;
        private Class<?>[] _types;
        private boolean _optional;

        /**
         * @return Returns the name.
         */
        protected String getName() {
            return _name;
        }
        /**
         * @param name The name to set.
         */
        protected void setName(String name) {
            _name = name;
        }
        /**
         * @return Returns the optional.
         */
        protected boolean isOptional() {
            return _optional;
        }
        /**
         * @param optional The optional to set.
         */
        protected void setOptional(boolean optional) {
            _optional = optional;
        }
        /**
         * @return Returns the type.
         */
        protected Class<?>[] getTypes() {
            return _types;
        }

        
        protected void setTypes(Class<?>[] types) {
            _types = types;
        }
        public Class<?> assignableAs(Object arg, boolean useConversion) {
            
            for (int idx=0; idx < _types.length; idx++) {
                Class<?> type = _types[idx];
                
                try {
                    arg = (Scriptable.class.isAssignableFrom(type) ? arg : Context.jsToJava(arg, (useConversion ? type : Object.class)));
                    if (type.isAssignableFrom(arg.getClass())) {
                        return type;
                    }
                }
                catch (EvaluatorException e) {
                    // Ignore as this is a "false" one
                }
            }
            
            return null;
         
        }
        
    }
    
    public static VarArgParser create(String functionName) {
        return new VarArgParser(functionName);
    }
    
    public VarArgParser(String functionName) {
        _functionName = functionName;
    }
    
    /**
     * Adds the definition of an argument
     * @param name Name of the argument (for retrieval)
     * @param type Type of the argument. Use native Java types if they are to be expected, no Javascript wrapper classes
     * @param optional Defines if this argument is optional and can be omitted
     * @return The vararg parser object, to continue adding parameters in this instruction
     */
    public VarArgParser add(String name, Class<?>[] types, boolean optional) {
        
        if (optional) {
            if (_mandatoryBlockStart != -1 && _mandatoryBlockEnd == -1) {
                _mandatoryBlockEnd = _arguments.size() - 1;
            }
        }
        else {
            if (_mandatoryBlockStart == -1) {
                _mandatoryBlockStart = _arguments.size();
            }
            else if (_mandatoryBlockEnd != -1) {
                throw new IllegalArgumentException("Only one mandatory block allowed");
            }
        }
        
        ArgDefinition arg = new ArgDefinition();
        arg.setName(name);
        arg.setTypes(types);
        arg.setOptional(optional);
        _arguments.add(arg);
        _combinations = null;
        return this;
    }
    
    /**
     * Adds the definition of an argument that can be of varying types
     * @param name Name of the argument (for retrieval)
     * @param type Allowed types for the argument. Use native Java types if they are to be expected, no Javascript wrapper classes
     * @param optional Defines if this argument is optional and can be omitted
     * @return The vararg parser object, to continue adding parameters in this instruction
     */
    public VarArgParser add(String name, Class<?> type, boolean optional) {
        return add(name, new Class[] {type}, optional);
    }
    
    /**
     * Adds the definition of a mandatory argument
     * @param name Name of the argument (for retrieval)
     * @param type Type of the argument. Use native Java types if they are to be expected, no Javascript wrapper classes
     * @return The vararg parser object, to continue adding parameters in this instruction
     */
    public VarArgParser add(String name, Class<?> type) {
        return add(name, type, false);
    }
    
    /**
     * Adds the definition of a mandatory argument that can be of varying types
     * @param name Name of the argument (for retrieval)
     * @param type Allowed types for the argument. Use native Java types if they are to be expected, no Javascript wrapper classes
     * @return The vararg parser object, to continue adding parameters in this instruction
     */
    public VarArgParser add(String name, Class<?>[] types) {
        return add(name, types, false);
    }
    
    public Arguments parse(Object[] args) throws IllegalArgumentException {
        
        if (_combinations == null) {
            throw new IllegalStateException("The method pack() was not called for this VarArgParser!");
        }
        
        // Try to find the matching arg combination without converting values
        Class<?>[] winningTypes = null;
        List<ArgDefinition> winningCombination = null;
        for (List<ArgDefinition> argCombination : _combinations) {
            if (winningTypes == null || argCombination.size() > winningTypes.length) {
                Class<?>[] types = matchingCombination(argCombination, args, false);
                if (types != null) {
                    winningTypes = types;
                    winningCombination = argCombination;
                }
            }
        }
        
        // If still none found try matching arg combination WITH converting values
        if (winningTypes == null) {
            for (List<ArgDefinition> argCombination : _combinations) {
                if (winningTypes == null || argCombination.size() > winningTypes.length) {
                    Class<?>[] types = matchingCombination(argCombination, args, true);
                    if (types != null) {
                        winningTypes = types;
                        winningCombination = argCombination;
                    }
                }
            }
        }
        
        if (winningTypes != null) {
            return parseWithDefinition(args, winningCombination, winningTypes);
        }
        else {
            List<String> types = new ArrayList<>();
                for (Object arg : args) {
                    types.add(arg != null ? arg.getClass().getName() : "(null)");
                }
            
            throw new IllegalArgumentException("Arguments do not match function signature of " + _functionName + ": " + WGUtils.serializeCollection(types, ", "));
        }
        
    }
    
    
    /**
     * We test here for integrity of the argument combination, according to the rules described on class documentation
     * 
     * @param argCombination
     * @return
     */
    private boolean isValidCombination(List<ArgDefinition> argCombination) {

        // Test parameters backward from the mandatory block
        if (_mandatoryBlockStart >  0) {
            
            boolean mustBeOff = false;
            for (int idx = _mandatoryBlockStart - 1; idx >= 0; idx--) {
                
                ArgDefinition def  = _arguments.get(idx);
                if (!argCombination.contains(def)) {
                    if (mustBeOff == false) {
                        mustBeOff = true;
                    }
                }
                else {
                    if (mustBeOff == true) {
                        return false;
                    }
                }
            }
        }
        
        // Test parameters onward from the mandatory block
        if (_mandatoryBlockEnd != -1 && _mandatoryBlockEnd < (_arguments.size() - 1)) {
            boolean mustBeOff = false;
            for (int idx = _mandatoryBlockEnd + 1; idx < _arguments.size(); idx++) {
                
                ArgDefinition def  = _arguments.get(idx);
                if (!argCombination.contains(def)) {
                    if (mustBeOff == false) {
                        mustBeOff = true;
                    }
                }
                else {
                    if (mustBeOff == true) {
                        return false;
                    }
                }
            }
        }
        
        return true;
        
        
    }

    /**
     * Tests if a combination of argument definitions and argument matches regarding their data types
     * @param argCombination
     * @param args
     * @return
     */
    private Class<?>[] matchingCombination(List<ArgDefinition> argCombination, Object[] args, boolean useConversion) {
        
        // No way to match if the definitions outnumber the arguments
        if (argCombination.size() > args.length) {
            return null;
        }
        
        Class<?>[] combination = new Class[argCombination.size()];
        for (int idx=0; idx < argCombination.size(); idx++) {
            
            ArgDefinition argDef = argCombination.get(idx);
            Object arg = args[idx];
            
            if (arg == null) {
                return null;
            }
            Class<?> type = argDef.assignableAs(arg, useConversion);
            if (type == null) {
                return null;
            }
            combination[idx] = type;
            
        }
        
        return combination;
        
    }

    private List<ArgDefinition> getArgumentCombination(int combinationBits) {
        
        int optionParamBit = 0;
        Iterator<ArgDefinition> args = _arguments.iterator();
        List<ArgDefinition> argCombination = new ArrayList<ArgDefinition>();
        
        while (args.hasNext()) {
            ArgDefinition argDef = (ArgDefinition) args.next();
            if (argDef.isOptional()) {
                if (optionParamBit == 0) {
                    optionParamBit = 1;
                }
                else {
                    optionParamBit *= 2;
                }
                if ((combinationBits & optionParamBit) != optionParamBit) {
                    argCombination.add(argDef);
                }
            }
            else {
                argCombination.add(argDef);
            }
        }
        
        return argCombination;
        
        
    }

    /**
     * Parses an array of arguments and returns an Arguments object for retrieval.
     * @param args The arguments array
     * @return An Arguments object for retrieval
     * @throws IllegalArgumentException if an argument was provided with an illegal type
     */
    public Arguments parseWithDefinition(Object[] args, List<ArgDefinition> argDefs, Class<?>[] combination) throws IllegalArgumentException {
        
        
        Map<String,Object> result = new HashMap<String,Object>();
        List<Object> overflow = new ArrayList<Object>();

        // Go thru arguments and argument definitions
        int argPos = 0;
        int argDefPos = 0;
        while (argPos < args.length) {
            
            Object arg = args[argPos];

            if (argDefPos >= combination.length) {
                
                if (isUnwrapOverflowArgs() && arg instanceof Scriptable) {
                    arg = Context.jsToJava(arg, Object.class);
                }
                
                overflow.add(arg);
                argPos++;
                continue;
            }
            
            Class<?> type = combination[argPos];
            arg = (Scriptable.class.isAssignableFrom(type) ? arg : Context.jsToJava(arg, type));
            
            ArgDefinition argDef = argDefs.get(argPos);
            result.put(argDef.getName(), arg);
            argPos++;
            argDefPos++;
            
        }
            
        return new Arguments(result, overflow);
        
    }

    /**
     * Determines if additional arguments that are not defined should be unwrapped
     * if they are wrapped java objects
     */
    public boolean isUnwrapOverflowArgs() {
        return _unwrapOverflowArgs;
    }

    /**
     * Set if additional arguments that are not defined should be unwrapped
     * if they are wrapped java objects
     */
    public VarArgParser setUnwrapOverflowArgs(boolean unwrapOverflowArgs) {
        _unwrapOverflowArgs = unwrapOverflowArgs;
        return this;
    }

    public String getFunctionName() {
        return _functionName;
    }
    
    /**
     * Prepares the vararg parser for usage
     * All parameters must have been added before
     */
    public VarArgParser pack() {
        
     // Count optional parameters. Determines the maximum value of the combination bitfield int
        int maxCombinationBits = 1;
        Iterator<ArgDefinition> argDefs = _arguments.iterator();
        while (argDefs.hasNext()) {
            ArgDefinition argDef = (ArgDefinition) argDefs.next();
            if (argDef.isOptional()) {
                maxCombinationBits *= 2;
            }
        }
        
        // Test all binary combinations of optional parameter usage for validity, store the valid ones
        List<List<ArgDefinition>> allowedCombinations = new ArrayList<List<ArgDefinition>>();
        for (int combinationBits=0; combinationBits < maxCombinationBits; combinationBits++) {
            List<ArgDefinition> argCombination = getArgumentCombination(combinationBits);
            if (isCompatibleMode() || isValidCombination(argCombination)) {
                allowedCombinations.add(argCombination);
            }
        }
        
        _combinations = allowedCombinations;
        
        return this;
        
    }

    public boolean isCompatibleMode() {
        return _compatibleMode;
    }

    public VarArgParser setCompatibleMode(boolean compatibleMode) {
        _compatibleMode = compatibleMode;
        return this;
    }
    


}
