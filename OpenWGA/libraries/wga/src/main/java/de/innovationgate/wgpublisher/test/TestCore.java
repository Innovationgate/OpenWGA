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
package de.innovationgate.wgpublisher.test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.test.log.ObjectFactory;
import de.innovationgate.wgpublisher.test.log.Testsuite;
import de.innovationgate.wgpublisher.test.log.Testsuite.Testcase;
import de.innovationgate.wgpublisher.test.log.Testsuite.Testcase.Failure;
import de.innovationgate.wgpublisher.test.log.Testsuites;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
/**
 * implements assert functions for tmlscript
 *
 */
public class TestCore {
    
    public static final String TESTCLASS_BASE_PACKAGE = "de.innovationgate.wga.test.";

    private JAXBContext _logContext; 
    
    private static boolean _javaAssertionsEnabled = false;
    static {
        assert _javaAssertionsEnabled = true;
    }
    
    public interface JavaAssertion {
        public void exec() throws Exception;
    }
    
    // map containing assertions mapped by category, value is a list of assertions
    private HashMap<String,List<Assertion>> _assertions = new HashMap<String,List<Assertion>>();
    
    // map containing preregistered assertions mapped by id;
    private HashMap<String,Assertion> _registeredAssertions = new HashMap<String,Assertion>();
    
    // registered assertions that have been executed
    private HashSet<String> _executedRegisteredAssertions = new HashSet<String>();
    
    // enable tests (true/false)
    private boolean _enabled;
    
    private boolean _logEnabled;
    
    // stop on assertions and start tmlscript:debugger
    private boolean _debugAssertions;
    
    private String _logDirPath;   
    
    private RhinoExpressionEngine _engine;
    
    private WGACore _core;
    
    private FileWriter _foutASCII;

    private File _logDir;
    
    public TestCore(WGACore core, boolean enabled, String logDir) throws JAXBException {
        _core = core;
        _enabled = enabled;
        _logDirPath = logDir;
        _logEnabled = true;
        init();
    }
    
    public TestCore(WGACore core, boolean enabled) throws JAXBException {
        _core = core;
        _enabled = enabled;
        _logEnabled = false;
        init();
    }
    
    private void init() throws JAXBException {
        // init expression engine
        _engine = ExpressionEngineFactory.getTMLScriptEngine();
        if (_engine == null) {
            throw new RuntimeException("cannot initialize tmlExpression-Engine");
        }         
        if (_logEnabled) {
            _logContext = JAXBContext.newInstance(Testsuites.class);
            if (_logDirPath != null) {
                _logDir = new File(_logDirPath);
                if (!_logDir.exists()) {
                    _logDir.mkdirs();
                }
            }
        }
    }
    
    public void assertTrue(String title, String category, String condition, TMLContext context) {
        if (!_enabled) {
            return;
        } else {
            // create assertion
            Assertion assertion = new Assertion(Assertion.TYPE_ASSERT_TRUE, title, category);
            doAssertTrue(condition, context, assertion);
        }                        
    }
    
    public void assertTrue(String id, String condition, TMLContext context) {
        if (!_enabled) {
            return;
        } else {
            // get preregistered assertion
            Assertion assertion = (Assertion) _registeredAssertions.get(id);
            if (assertion != null) {                             
                doAssertTrue(condition, context, assertion);
                // remove assertion
                _registeredAssertions.remove(id); 
                _executedRegisteredAssertions.add(id);
            }
            else {
                assertTrue("Tried to execute a non-existing predefined assertion of id: " + id, "Internal", "false", context);
            }
        }                        
    }    

    private void doAssertTrue(String condition, TMLContext context, Assertion assertion) {
        
        if (context != null) {
            try {
                assertion.setContextPath(context.getpath());
            }
            catch (WGAPIException e) {
                assertion.setContextPath("Unable to retrieve contextpath bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            }
        }
        else {
	        assertion.setContextPath("(none)");
        }
        
        assertion.setExpression(condition);
        // evaluate expression
        ExpressionResult result = _engine.evaluateExpression(condition, context, ExpressionEngine.TYPE_EXPRESSION, null);
        if (result.isError()) {
            assertion.setResult(false);
            assertion.setExpressionError(true);   
            assertion.setExpressionErrorMsg(result.getException().getMessage());
            // break if debugmode enabled
            if (_debugAssertions) {
                _engine.debug();
            }
        } else {
            assertion.setResult(result.isTrue());
            // break if result is false and debugmode enabled 
            if (result.isFalse() && _debugAssertions) {
                _engine.debug();
            }
        }
         
        assertion.setTime(new Date(System.currentTimeMillis()));
        
        determineTmlModule(context, assertion);
        
        assertion.setExecuted(true);
        addAssertion(assertion);
    }

    protected void determineTmlModule(TMLContext context, Assertion assertion) {
        if (context != null) {
            if (context.getvar("$testmodule") != null) {
                assertion.setTmlModule((String) context.getvar("$testmodule"));
            }
            else  {
                try {
                    assertion.setTmlModule(WGA.get(context).design().getBaseReference().getResourceName());
                }
                catch (WGException e) {
                    context.getlog().error("Exception determining WebTML module for assertion", e);
                }
            }
        }
    }
    
    public boolean assertEquals(String title, String category, Object obj1, Object obj2, TMLContext context) {
        if (!_enabled) {
            return false;
        } else {
            // create assertion
            Assertion assertion = new Assertion(Assertion.TYPE_ASSERT_EQUALS, title, category);
            return doAssertEquals(obj1, obj2, context, assertion);                          
        }
    }
    
    public <T extends Comparable<T>> boolean assertLarger(String title, String category, T obj1, T obj2, TMLContext context) {
        if (!_enabled) {
            return false;
        } else {
            // create assertion
            Assertion assertion = new Assertion(Assertion.TYPE_ASSERT_LARGER, title, category);
            return doAssertLarger(obj1, obj2, context, assertion);                          
        }
    }
    
    public boolean assertNotEquals(String title, String category, Object obj1, Object obj2, TMLContext context) {
        if (!_enabled) {
            return false;
        } else {
            // create assertion
            Assertion assertion = new Assertion(Assertion.TYPE_ASSERT_NOT_EQUALS, title, category);
            return doAssertNotEquals(obj1, obj2, context, assertion);                          
        }
    }
    
    public void assertEquals(String id, Object obj1, Object obj2, TMLContext context) {
        if (!_enabled) {
            return;
        } else {
            // get preregistered assertion
            Assertion assertion = (Assertion) _registeredAssertions.get(id);
            if (assertion != null) {            
                doAssertEquals(obj1, obj2, context, assertion);
                // remove assertion
                _registeredAssertions.remove(id);      
                _executedRegisteredAssertions.add(id);
            }
            else {
                assertTrue("Tried to execute a non-existing predefined assertion of id: " + id, "Internal", "false", context);
            }
            
        }
    }    

    
    
    private boolean doAssertEquals(Object obj1, Object obj2, TMLContext context, Assertion assertion) {
        
        if (context != null) {
            try {
                assertion.setContextPath(context.getpath());
            }
            catch (WGAPIException e) {
                assertion.setContextPath("Unable to retrieve contextpath bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            }
        }
        else {
            assertion.setContextPath("(none)");
        }
        
        assertion.setTime(new Date(System.currentTimeMillis()));
        
        // On date objects we strip of the millis bc. dates from databases seldomly have millisecond precision
        // Date comparisions therefor need to ensure that difference > seconds for false assertions
        if (obj1 instanceof Date) {
            obj1 = new Date(WGUtils.cutoffTimeMillis(((Date) obj1).getTime()));
        }
        if (obj2 instanceof Date) {
            obj2 = new Date(WGUtils.cutoffTimeMillis(((Date) obj2).getTime()));
        }

        assertion.setValueObj1(obj1);
        assertion.setValueObj2(obj2);
        
        if (obj1 != null) {          
            // test for equals
            if (obj2 == null) {
                assertion.setResult(false);
            }
            else {
                RhinoExpressionEngine scriptEngine = ExpressionEngineFactory.getTMLScriptEngine();
                int scriptType1 = scriptEngine.determineTMLScriptType(obj1);
                int scriptType2 = scriptEngine.determineTMLScriptType(obj2);
                if (scriptType1 != RhinoExpressionEngine.TYPE_NOTMLSCRIPT && scriptType2 != RhinoExpressionEngine.TYPE_NOTMLSCRIPT) {
                    assertion.setResult(scriptEngine.scriptableEquals(obj1, obj2));
                }
                else {
                    assertion.setResult(obj1.equals(obj2));
                }
            }
            // break if result is false and debugmode enabled 
            if (!assertion.isResult() && _debugAssertions) {
                _engine.debug();
            }
        } else {
            if (obj2 == null) {
                assertion.setResult(true);
            } else {
                assertion.setResult(false);
                // break if debugmode enabled
                if (_debugAssertions) {
                    _engine.debug();
                }
            }
        }
        
        determineTmlModule(context, assertion);
        
        assertion.setExecuted(true);
        addAssertion(assertion);
        
        return assertion.isResult();
    }
    
    @SuppressWarnings("unchecked")
    private <T extends Comparable<T>> boolean doAssertLarger(T obj1, T obj2, TMLContext context, Assertion assertion) {
        
        if (context != null) {
            try {
                assertion.setContextPath(context.getpath());
            }
            catch (WGAPIException e) {
                assertion.setContextPath("Unable to retrieve contextpath bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            }
        }
        else {
            assertion.setContextPath("(none)");
        }
        
        assertion.setTime(new Date(System.currentTimeMillis()));
        
        // On date objects we strip of the millis bc. dates from databases seldomly have millisecond precision
        // Date comparisions therefor need to ensure that difference > seconds for false assertions
        if (obj1 instanceof Date) {
            obj1 = (T) new Date(WGUtils.cutoffTimeMillis(((Date) obj1).getTime()));
        }
        if (obj2 instanceof Date) {
            obj2 = (T) new Date(WGUtils.cutoffTimeMillis(((Date) obj2).getTime()));
        }

        assertion.setValueObj1(obj1);
        assertion.setValueObj2(obj2);
        
        if (obj1 != null) {          
            // test for equals
            if (obj2 == null) {
                assertion.setResult(false);
            }
            else {
                assertion.setResult(obj1.compareTo(obj2) > 0);
            }
            
            
            // break if result is false and debugmode enabled 
            if (!assertion.isResult() && _debugAssertions) {
                _engine.debug();
            }
        } else {
            assertion.setResult(false);
        }
        
        determineTmlModule(context, assertion);
        
        assertion.setExecuted(true);
        addAssertion(assertion);
        
        return assertion.isResult();
    }
    
    private boolean doAssertNotEquals(Object obj1, Object obj2, TMLContext context, Assertion assertion) {
        
        if (context != null) {
            try {
                assertion.setContextPath(context.getpath());
            }
            catch (WGAPIException e) {
                assertion.setContextPath("Unable to retrieve contextpath bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            }
        }
        else {
            assertion.setContextPath("(none)");
        }
        
        assertion.setTime(new Date(System.currentTimeMillis()));

        assertion.setValueObj1(obj1);
        assertion.setValueObj2(obj2);
        
        if (obj1 != null) {          
            // test for equals            
            if (obj2 == null) {
                assertion.setResult(true);
            }
            else {
                assertion.setResult(!obj1.equals(obj2));
            }
            
            // break if result is false and debugmode enabled 
            if (!assertion.isResult() && _debugAssertions) {
                _engine.debug();
            }
        } else {
            if (obj2 == null) {
                assertion.setResult(false);
            } else {
                assertion.setResult(true);
                // break if debugmode enabled
                if (_debugAssertions) {
                    _engine.debug();
                }
            }
        }
        
        determineTmlModule(context, assertion);
        
        assertion.setExecuted(true);
        addAssertion(assertion);
        
        return assertion.isResult();
    }
    
    public boolean assertJava(String title, String category, JavaAssertion javaAssertion) {
        
        if (!_enabled) {
            return true;
        }
            
        // create assertion
        Assertion assertion = new Assertion(Assertion.TYPE_ASSERT_JAVA, title, category);
        assertion.setContextPath("");
        assertion.setExpression("(java code)");

//          Test enablement of java assertions
        if (_javaAssertionsEnabled) {
        
            // execute java assertion
            try {
               javaAssertion.exec();
               assertion.setResult(true);
            }
            catch (AssertionError e) {
                assertion.setResult(false);
                assertion.setExpression(e.getMessage());
            }
            catch (Exception e) {
                assertion.setResult(false);
                assertion.setExpressionError(true);
                assertion.setExpressionErrorMsg(e.getMessage() + " (" + e.getClass().getName() + ")");
            }
            
        }
        else {
            assertion.setResult(false);
            assertion.setExpression("Java Assertions are disabled. This test will not work");
        }
       
        assertion.setTime(new Date(System.currentTimeMillis()));
        
        
        Class<?> enclosingClass = javaAssertion.getClass().getEnclosingClass();
        if (enclosingClass != null && enclosingClass.getName().startsWith(TESTCLASS_BASE_PACKAGE)) {
            assertion.setTmlModule(WGUtils.strReplace(enclosingClass.getName().substring(TESTCLASS_BASE_PACKAGE.length()), ".", ":", true));
        }
        
        addAssertion(assertion);
        return assertion.isResult();
         
        
    }
    
    /**
     * preregisters an assertion with result==false by given id 
     * @param id
     */
    public void registerAssertTrue(String id, String title, String category) {  
        Assertion assertion = new Assertion(Assertion.TYPE_ASSERT_TRUE, title, category, id);
        assertion.setResult(false);
        assertion.setTime(new Date(System.currentTimeMillis()));
        assertion.setPreregistered(true);
        _registeredAssertions.put(id, assertion);
    }
    
    /**
     * preregisters an assertion with result==false by given id 
     * @param id
     */
    public void registerAssertEquals(String id, String title, String category) {  
        Assertion assertion = new Assertion(Assertion.TYPE_ASSERT_EQUALS, title, category, id);
        assertion.setResult(false);
        assertion.setTime(new Date(System.currentTimeMillis()));
        assertion.setPreregistered(true);
        _registeredAssertions.put(id, assertion);
    }
    
    
    
    private void addAssertion(Assertion assertion) {
        // add to map, if category not exist - create
        String cat = assertion.getCategory();
        List<Assertion> assertList = getAssertionsForCategory(cat);  
        assertList.add(assertion);
        
        // if log enabled refresh filesystem
        if (_logEnabled) {
            logAssertion(assertion);          
        }
    }

    private void logAssertion(Assertion assertion) {

        try {
            String testsuitesName;
            String testsuiteName;
            String tmlModule = assertion.getTmlModule();
            if (tmlModule != null) {
                tmlModule = tmlModule.toLowerCase();
                int colonPos = tmlModule.indexOf(":");
                if (colonPos != -1) {
                    testsuitesName = tmlModule.substring(0, colonPos);
                    testsuiteName = tmlModule.substring(colonPos + 1);
                }
                else {
                    testsuitesName = tmlModule;
                    testsuiteName = "default";
                }
            }
            else {
                testsuitesName = "default";
                testsuiteName = "default";
            }
            
            Testsuites suites;
            File suitesFile = new File(_logDir, testsuitesName + ".xml");
            if (suitesFile.exists()) {
                suites = (Testsuites) _logContext.createUnmarshaller().unmarshal(suitesFile);
            }
            else {
                suitesFile.createNewFile();
                suites = new ObjectFactory().createTestsuites();
                suites.setName(testsuitesName);
            }
            
            Testsuites.Testsuite suite = null;
            for (Testsuites.Testsuite existingSuite : suites.getTestsuite()) {
                if (existingSuite.getName().equals(testsuiteName)) {
                    suite = existingSuite;
                    break;
                }
            }
            if (suite == null) {
                suite = new ObjectFactory().createTestsuitesTestsuite();
                suite.setName(testsuiteName);
                suite.setHostname(System.getProperty(_core.getWgaConfiguration().getServerName()));
                suites.getTestsuite().add(suite);
            }
            
            Testcase testCase = new ObjectFactory().createTestsuiteTestcase();
            suite.getTestcase().add(testCase);
            testCase.setClassname(testsuiteName.replace(":", "."));
            testCase.setName(assertion.getTitle());
            testCase.setTime(new BigDecimal(0));
            if (assertion.isExpressionError()) {
                Testsuite.Testcase.Error error = new ObjectFactory().createTestsuiteTestcaseError();
                error.setMessage(assertion.getExpressionErrorMsg());
                testCase.setError(error);
                suite.setErrors(suite.getErrors() + 1);
            }
            else if (!assertion.isResult()) {
                Failure failure = new ObjectFactory().createTestsuiteTestcaseFailure();
                failure.setMessage(assertion.getFailureMessage());
                failure.setValue(XStreamUtils.XSTREAM_CLONING.toXML(assertion));
                testCase.setFailure(failure);
                suite.setFailures(suite.getFailures() + 1);
            }
            
            suite.setTests(suite.getTests()+1);
            
            _logContext.createMarshaller().marshal(suites, suitesFile);
            
            
        }
        catch (Exception e) {
            _core.getLog().error("Exception logging test assertion", e);
        }
        
    }

    public List<Assertion> getAssertionsForCategory(String cat) {
        List<Assertion> assertList = _assertions.get(cat);
        if (assertList == null) {
            assertList = new ArrayList<Assertion>();
            _assertions.put(cat, assertList);            
        }
        return assertList;
    }

    public boolean isEnabled() {
        return _enabled;
    }

    public void finalize() {
        try {
            _foutASCII.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    public List<Assertion> getAllAssertions() {
        
        List<Assertion> list = new ArrayList<Assertion>();
        List<String> assertCategories = new ArrayList<String>(_assertions.keySet());
        //Collections.sort(assertCategories);
        
        
        Iterator<String> assertCatIt = assertCategories.iterator();
        List<Assertion> assertList;
        while (assertCatIt.hasNext()) {
            assertList = _assertions.get(assertCatIt.next());
            list.addAll(assertList);
        }
        return list;
        
    }
    
    public void reset() throws JAXBException {
        _assertions.clear();
        _registeredAssertions.clear();
        _executedRegisteredAssertions.clear();
        init();
    }
    
    /**
     * put not executed but registered assertions in _assertions-Map
     *
     */
    public void testDone() {        
        Iterator<Assertion> it = _registeredAssertions.values().iterator();
        while (it.hasNext()) {
            Assertion assertion = (Assertion) it.next();
            assertion.setExpressionErrorMsg("Was registered by key '" + assertion.getId() + " but never executed.");
            addAssertion(assertion);
        }
        _registeredAssertions.clear();
        _executedRegisteredAssertions.clear();
    }

    public boolean isDebugAssertions() {
        return _debugAssertions;
    }

    public void setDebugAssertions(boolean debugAssertions) {
        _debugAssertions = debugAssertions;
    }

    public boolean assertIsRegisteredOrExecuted(String id) {
        return _registeredAssertions.containsKey(id) || _executedRegisteredAssertions.contains(id);        
    }
    
    public boolean assertIsRegistered(String id) {
        return _registeredAssertions.containsKey(id);
    }

    public boolean isLogEnabled() {
        return _logEnabled;
    }

    public void setLogEnabled(boolean logEnabled) {
        _logEnabled = logEnabled;
    }
}
