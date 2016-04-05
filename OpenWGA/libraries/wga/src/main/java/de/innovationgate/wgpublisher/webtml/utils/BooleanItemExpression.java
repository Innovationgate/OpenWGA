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

package de.innovationgate.wgpublisher.webtml.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;

public class BooleanItemExpression {
    
    class AndOperator implements Operator {
        
        @Override
        public boolean isTrue(List<BooleanTerm> terms, TMLContext cx, List<Object> equalityObjects) throws WGException {

            for (BooleanTerm t : terms) {
                if (!t.isTrue(cx, equalityObjects)) {
                    return false;
                }
            }
            
            return true;
            
        }

        @Override
        public String toString() {
            return "&";
        }

    }
    
    interface BooleanTerm extends Term {
        public boolean isTrue(TMLContext cx, List<Object> equalityObjects) throws WGException;
    }
    
    
    class CloseBracket implements Term {
        
        @Override
        public String toString() {
            return ")";
        }
        
    }
    
    class Expression implements BooleanTerm {
        
        private List<Term> _terms = new ArrayList<BooleanItemExpression.Term>();
        private Expression _parent;
        
        public Expression(Expression parent) {
            _parent = parent;
        }
        
        public void addTerm(Term term) {
            _terms.add(term);
        }

        public Expression getParent() {
            return _parent;
        }

        public List<Term> getTerms() {
            return _terms;
        }
        
        @Override
        public boolean isTrue(TMLContext cx, List<Object> equalityObjects) throws WGException {

            Operator operator = null;
            List<BooleanTerm> booleanTerms = new ArrayList<BooleanItemExpression.BooleanTerm>();
            boolean nextTermIsNegated = false;
            for (Term t : _terms) {
                if (t instanceof Negation) {
                    nextTermIsNegated = !nextTermIsNegated;
                }
                
                else if (t instanceof Operator) {
                    if (nextTermIsNegated) {
                        throw new WGIllegalArgumentException("Cannot negate operator");
                    }
                    if (operator == null) {
                        operator = (Operator) t;
                    }
                    else if (operator.getClass() != t.getClass()) {
                        throw new WGIllegalArgumentException("Multiple types of operators in (sub) expression: " + operator.toString() + ", " + t.toString());
                    }
                }
                else if (t instanceof BooleanTerm) {
                    if (nextTermIsNegated) {
                        booleanTerms.add(new NegatedBooleanTerm((BooleanTerm) t));
                        nextTermIsNegated = false;
                    }
                    else {
                        booleanTerms.add((BooleanTerm) t);
                    }
                }
            }
            
            if (operator == null) {
                operator = new AndOperator();
            }
            
            return operator.isTrue(booleanTerms, cx, equalityObjects);
            
        }

        @Override
        public String toString() {

            StringBuilder b = new StringBuilder();
            b.append("(");
            for (Term t : _terms) {
                b.append(t.toString());
            }
            b.append(")");
            return b.toString();
        }
        
    }
    
    class ItemTerm implements BooleanTerm {
        
        private String _name;

        public ItemTerm(String itemName) {
            _name = itemName;
        }

        public String getName() {
            return _name;
        }
        
        @Override
        public boolean isTrue(TMLContext cx, List<Object> equalityObjects) throws WGException {
            if (equalityObjects != null) {
                Object itemValue = cx.item(_name);
                for (Object obj : equalityObjects) {
                    
                    if (obj instanceof Number && itemValue instanceof Number) {
                        obj = WGUtils.toBigDecimal((Number) obj);
                        itemValue = WGUtils.toBigDecimal((Number) itemValue);
                    }
                    if (WGUtils.nullSafeEquals(obj, itemValue)) {
                        return true;
                    }
                    
                }
                return false;
            }
            else {
                return cx.istrue(_name);
            }
        }

        @Override
        public String toString() {
            return _name;
        }
        
    }
    
    class NegatedBooleanTerm implements BooleanTerm {
        
        private BooleanTerm _originalTerm;
        
        public NegatedBooleanTerm(BooleanTerm originalTerm) {
            _originalTerm = originalTerm;
        }
        
        @Override
        public boolean isTrue(TMLContext cx, List<Object> equalityObjects) throws WGException {
            return !_originalTerm.isTrue(cx, equalityObjects);
        }
        
    }
    
    class Negation implements Term {
      
        @Override
        public String toString() {
            return "!";
        }
        
    }
    
    class OpenBracket implements Term {
        
        @Override
        public String toString() {
            return "(";
        }
    }
    
    interface Operator extends Term {
        public boolean isTrue(List<BooleanTerm> terms, TMLContext cx, List<Object> equalityObjects) throws WGException;
    }
    
    class OrOperator implements Operator {
        @Override
        public boolean isTrue(List<BooleanTerm> terms, TMLContext cx, List<Object> equalityObjects) throws WGException {
            for (BooleanTerm t : terms) {
                if (t.isTrue(cx, equalityObjects)) {
                    return true;
                }
            }
            
            return false;
        }
        
        @Override
        public String toString() {
            return "|";
        }
    }
    
    interface Term {
    }

    
    private Expression _expression;
    private List<String> _equalityItemExpressions = null;
    private boolean _onePart;

    public BooleanItemExpression(String expressionStr) throws WGIllegalArgumentException {
        _onePart = true;
        parseEvaluationPart(expressionStr);
    }
    
    public BooleanItemExpression(String evaluation, String equation) throws WGIllegalArgumentException {
        _onePart = false;
        parseEvaluationPart(evaluation);
        parseEquationPart(equation);
    }

    private void parseEvaluationPart(String expressionStr) throws WGIllegalArgumentException {
        Expression rootExpression = new Expression(null);
        Expression expression = rootExpression;
        StringBuilder currentItemTerm = null;
        String equationPart = null;
        
        int idx=-1;
        for (char c : expressionStr.toCharArray()) {
            idx++;
            
            Term nextTerm = null;
            if (Character.isWhitespace(c)) {
                continue;
            }
            
            if (c == '\'') {
                throw new WGIllegalArgumentException("String literals are not allowed in the evaluation part of boolean item expressions: " + expressionStr);   
            }
            
            if (c == '&') {
                nextTerm = new AndOperator();
            }
            else if (c == '|') {
                nextTerm  = new OrOperator();
            }
            else if (c == '(') {
                nextTerm = new OpenBracket();
            }
            else if (c == ')') {
                nextTerm = new CloseBracket();
            }
            else if (c == '!') {
                nextTerm = new Negation();
            }
            
            // The equation part begins, cancel evaluation and use the rest of the string to parse equality objects
            else if (c == '=') {
                if (!_onePart) {
                    throw new WGIllegalArgumentException("Equation part is not allowed in this item expression, as it comes from a different input");
                }
                
                equationPart = expressionStr.substring(idx +1);
                break;
            }
            
            // No new term. Start/continue writing the current item term
            if (nextTerm == null) {
                if (currentItemTerm == null) {
                    currentItemTerm = new StringBuilder();
                }
                currentItemTerm.append(c);
                continue;
            }
            
            // A new term. First close the current item term, if available.
            if (currentItemTerm != null) {
                expression.addTerm(new ItemTerm(currentItemTerm.toString()));
                currentItemTerm = null;
            }
            
            // Execute term operation
            if (nextTerm instanceof AndOperator || nextTerm instanceof OrOperator || nextTerm instanceof Negation) {
                expression.addTerm(nextTerm);
                continue;
            }
            
            // Opening bracket: Open a sub expression and continue working on it
            if (nextTerm instanceof OpenBracket) {
                Expression subExpression = new Expression(expression);
                expression.addTerm(subExpression);
                expression = subExpression;
            }

            // Closing bracket: Return to parent expression
            if (nextTerm instanceof CloseBracket) {
                if (expression.getParent() != null) {
                    expression = expression.getParent();
                }
                else {
                    throw new WGIllegalArgumentException("Invalid bracket cascading in boolean item expression: " + expressionStr);
                }
            }
            
        }
        
        // Close a still open term
        // A new term. First close the current item term, if available.
        if (currentItemTerm != null) {
            expression.addTerm(new ItemTerm(currentItemTerm.toString()));
            currentItemTerm = null;
        }
        
        if (expression != rootExpression) {
            throw new WGIllegalArgumentException("Invalid bracket cascading in boolean item expression: " + expressionStr);
        }
        
        _expression = expression;
        if (equationPart != null) {
            parseEquationPart(equationPart);
        }
    }
    
    private void parseEquationPart(String equationPart) {
        _equalityItemExpressions = WGUtils.deserializeCollection(equationPart.trim(), ",", true, '\'', false);
    }

    public boolean isTrue(TMLContext cx) throws WGException {
        
        List<Object> equalityObjects = null;
        if (_equalityItemExpressions != null) {
            equalityObjects = new ArrayList<Object>();
            for (String itemExpression : _equalityItemExpressions) {
                equalityObjects.add(cx.item(itemExpression));
            }
        }
        
        return _expression.isTrue(cx, equalityObjects);
    }
    
    public boolean equalsValue(TMLContext cx, Object value) throws WGException {
        return _expression.isTrue(cx, Collections.singletonList(value));
    }
    
    public boolean equalsValues(TMLContext cx, List<Object> values) throws WGException {
        return _expression.isTrue(cx, values);
    }
    
    @Override
    public String toString() {
        return _expression.toString();
    }

}
