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

package de.innovationgate.wgpublisher;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.dom4j.Attribute;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentFactory;
import org.dom4j.Element;
import org.dom4j.io.DOMWriter;
import org.dom4j.io.XMLWriter;

import de.innovationgate.wgpublisher.webtml.utils.HttpErrorException;

public class WebTMLDebugger {

    /**
     * 
     */
    private WGPDispatcher _dispatcher;

    /**
     * @param wgpDispatcher
     */
    WebTMLDebugger(WGPDispatcher wgpDispatcher) {
        _dispatcher = wgpDispatcher;
    }

    private Transformer _debugModulesTransformer;

    private Transformer _debugTagsTransformer = null;

    private Transformer _debugPortletsTransformer;

    private Transformer getDebugModulesTransformer(boolean throwAway) throws TransformerConfigurationException, TransformerFactoryConfigurationError {

        if (_debugModulesTransformer == null || throwAway) {
            _debugModulesTransformer = TransformerFactory.newInstance().newTransformer(new StreamSource(_dispatcher.getServletContext().getResourceAsStream("/tmldebugModules.xsl")));
        }
        return _debugModulesTransformer;
    }
    
    private Transformer getDebugPortletsTransformer(boolean throwAway) throws TransformerConfigurationException, TransformerFactoryConfigurationError {

        if (_debugPortletsTransformer == null || throwAway) {
            _debugPortletsTransformer = TransformerFactory.newInstance().newTransformer(new StreamSource(_dispatcher.getServletContext().getResourceAsStream("/tmldebugPortlets.xsl")));
        }
        return _debugPortletsTransformer;
    }

    private Transformer getDebugTagsTransformer(boolean throwAway) throws TransformerConfigurationException, TransformerFactoryConfigurationError {

        if (_debugTagsTransformer == null || throwAway) {
            _debugTagsTransformer = TransformerFactory.newInstance().newTransformer(new StreamSource(_dispatcher.getServletContext().getResourceAsStream("/tmlDebugTags.xsl")));
        }
        return _debugTagsTransformer;
    }

    /**
     * @param request
     * @param response
     * @param session
     * @throws HttpErrorException
     * @throws IOException
     * @throws ServletException
     */
    void performDebugMode(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws HttpErrorException, ServletException, IOException {

        if (!_dispatcher.getCore().isAdminLoggedIn(request)) {
            throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, "You must be logged in to the wga admin page to use WebTML debugging", null);
        }

        String command = request.getParameter("command");
        Boolean debugModeEnabled = (Boolean) session.getAttribute(WGACore.ATTRIB_TMLDEBUG);
        if (debugModeEnabled == null) {
            debugModeEnabled = new Boolean(false);
        }
        Boolean resultTracingEnabled = (Boolean) session.getAttribute(WGACore.ATTRIB_TMLDEBUG_TRACE_RESULTS);
        if (resultTracingEnabled == null) {
            resultTracingEnabled = new Boolean(false);
        }

        Boolean optionsTracingEnabled = (Boolean) session.getAttribute(WGACore.ATTRIB_TMLDEBUG_TRACE_OPTIONS);
        if (optionsTracingEnabled == null) {
            optionsTracingEnabled = new Boolean(false);
        }

        Boolean tmlscriptOptimizationDisabled = (Boolean) session.getAttribute(WGACore.ATTRIB_TMLDEBUG_DISABLE_TMLSCRIPT_OPTIMIZATION);
        if (tmlscriptOptimizationDisabled == null) {
            tmlscriptOptimizationDisabled = new Boolean(false);
        }

        if (command == null) {
            request.getRequestDispatcher("/tmlDebugFrameset.jsp").include(request, response);
            return;
        }

        if (command.equalsIgnoreCase("toggledebug")) {
            debugModeEnabled = new Boolean(!debugModeEnabled.booleanValue());
            session.setAttribute(WGACore.ATTRIB_TMLDEBUG, debugModeEnabled);
            command = "status";
        }
        else if (command.equalsIgnoreCase("toggleresulttrace")) {
            resultTracingEnabled = new Boolean(!resultTracingEnabled.booleanValue());
            session.setAttribute(WGACore.ATTRIB_TMLDEBUG_TRACE_RESULTS, resultTracingEnabled);
            command = "status";
        }
        else if (command.equalsIgnoreCase("toggleoptionstrace")) {
            optionsTracingEnabled = new Boolean(!optionsTracingEnabled.booleanValue());
            session.setAttribute(WGACore.ATTRIB_TMLDEBUG_TRACE_OPTIONS, optionsTracingEnabled);
            command = "status";
        }
        else if (command.equalsIgnoreCase("toggletmlscriptoptimization")) {
            tmlscriptOptimizationDisabled = new Boolean(!tmlscriptOptimizationDisabled.booleanValue());
            session.setAttribute(WGACore.ATTRIB_TMLDEBUG_DISABLE_TMLSCRIPT_OPTIMIZATION, tmlscriptOptimizationDisabled);
            command = "status";
        }

        else if (command.equalsIgnoreCase("clearlist")) {
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            debugDocuments.clear();
            command = "list";
        }

        if (command.equalsIgnoreCase("toolbar")) {
            request.getRequestDispatcher("/tmlDebugToolbar.jsp").include(request, response);
        }
        else if (command.equalsIgnoreCase("status")) {
            response.getWriter().write("TMLScript stack traces are switched " + (tmlscriptOptimizationDisabled.booleanValue() ? "ON" : "OFF") + "\n");
            if (debugModeEnabled.booleanValue() == true) {
                response.getWriter().write("WebTML debug mode is switched ON\n");
                response.getWriter().write("Result tracing is switched " + (resultTracingEnabled.booleanValue() ? "ON" : "OFF") + "\n");
                response.getWriter().write("Options tracing is switched " + (optionsTracingEnabled.booleanValue() ? "ON" : "OFF") + "\n");
            }
            else {
                response.getWriter().write("WebTML debug mode is switched OFF\n");
            }

        }
        else if (command.equalsIgnoreCase("list")) {
            if (debugModeEnabled.booleanValue() == true) {
                sendDebugDocList(request, response, session);
            }
            else {
                throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "WebTML debug mode is not enabled. First enable it via tmldebug?command=on", null);
            }
        }
        else if (command.equalsIgnoreCase("showModules")) {
            showTMLModules(request, response, session);
        }
        else if (command.equalsIgnoreCase("showPortlets")) {
            showTMLPortlets(request, response, session);
        }
        else if (command.equalsIgnoreCase("showTags")) {
            showTMLTags(request, response, session);
        }
        else if (command.equalsIgnoreCase("show")) {
            sendDebugDoc(request, response, session);
        }
        else {
            throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "Unknown debug command: " + command, null);
        }

    }

    /**
     * @param request
     * @param response
     * @param session
     * @throws HttpErrorException
     * @throws IOException
     */
    private void sendDebugDoc(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws HttpErrorException, IOException {
        String urlStr = request.getParameter("url");
        String indexStr = request.getParameter("index");

        if (urlStr != null) {
            urlStr = _dispatcher.getCore().getURLEncoder().decode(urlStr);
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            Document debugDoc;
            for (int idx = 0; idx < debugDocuments.size(); idx++) {
                debugDoc = (Document) debugDocuments.get(idx);
                if (debugDoc.getRootElement().attributeValue("url", "").equals(urlStr)) {
                    indexStr = String.valueOf(idx);
                    break;
                }
            }
        }

        if (indexStr != null) {
            int index = Integer.valueOf(indexStr).intValue();
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            if (index == -1) {
                index = debugDocuments.size() - 1;
            }
            if (index >= debugDocuments.size()) {
                throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "Index out of range: " + index + " where maximum index is " + (debugDocuments.size() - 1), null);
            }
            else {
                Document doc = (Document) debugDocuments.get(index);
                response.setContentType("text/xml");
                XMLWriter writer = new XMLWriter(response.getWriter());
                writer.write(doc);
            }
        }
        else {
            throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "You must include either parameter index or url to address the debug document to show", null);
        }
    }

    /**
     * @param response
     * @param session
     * @throws IOException
     */
    private void sendDebugDocList(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws IOException {
        List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
        Document debugDoc;
        response.setContentType("text/html");
        Writer out = response.getWriter();
        out.write("<html><body>");
        out.write("<h2>Debug data for session " + session.getId() + "</h2>");
        out.write("<table width='100%' border='1'>");
        out.write("<thead><th>index</th><th>url</th><th>ajax</th><th>started</th><th>ended</th></thead>");
        out.write("<tbody>");

        for (int idx = debugDocuments.size() - 1; idx > -1; idx--) {
            debugDoc = (Document) debugDocuments.get(idx);
            out.write("<tr>");
            out.write("<td>" + String.valueOf(idx) + "</td>");
            out.write("<td><a target=\"blank\" href=\"" + _dispatcher.getContextPath() + "/tmlDebug?command=showModules&index=" + idx + "\">" + debugDoc.selectSingleNode("/tmldebugdocument/@url").getText()
                    + "</a></td>");
            out.write("<td>"+ (debugDoc.selectSingleNode("/tmldebugdocument/@ajax").getText().equals("true") ? "X" : "") + "</td>");
            out.write("<td>" + debugDoc.selectSingleNode("/tmldebugdocument/@started").getText() + "</td>");
            out.write("<td>");
            Attribute endedElement = (Attribute) debugDoc.selectSingleNode("/tmldebugdocument/@ended");
            if (endedElement != null) {
                out.write(endedElement.getText());
            }
            else {
                out.write("&nbsp;");
            }
            out.write("</td></tr>");
        }

        out.write("</tbody></table>");
        out.write("<button onclick=\"location.href='" + request.getContextPath() + "/tmlDebug?command=clearlist'\">Clear list</button>");
        out.write("</body></html>");
    }

    private void showTMLModules(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws HttpErrorException, IOException {
        String urlStr = request.getParameter("url");
        String indexStr = request.getParameter("index");

        if (urlStr != null) {
            urlStr = _dispatcher.getCore().getURLEncoder().decode(urlStr);
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            Document debugDoc;
            for (int idx = 0; idx < debugDocuments.size(); idx++) {
                debugDoc = (Document) debugDocuments.get(idx);
                if (debugDoc.getRootElement().attributeValue("url", "").equals(urlStr)) {
                    indexStr = String.valueOf(idx);
                    break;
                }
            }
        }

        response.setContentType("text/html");

        if (indexStr != null) {
            int index = Integer.valueOf(indexStr).intValue();
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            if (index == -1) {
                index = debugDocuments.size() - 1;
            }
            if (index >= debugDocuments.size()) {
                throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "Index out of range: " + index + " where maximum index is " + (debugDocuments.size() - 1), null);
            }
            else {
                Document doc = (Document) debugDocuments.get(index);
                doc.getRootElement().addAttribute("index", String.valueOf(index));

                try {
                    DOMWriter domWriter = new DOMWriter();
                    org.w3c.dom.Document domDocument = domWriter.write(doc);

                    Transformer trans = getDebugModulesTransformer(request.getParameter("throwAway") != null);
                    trans.transform(new DOMSource(domDocument), new StreamResult(response.getOutputStream()));
                }
                catch (TransformerConfigurationException e) {
                    response.sendError(500, e.getMessageAndLocation());
                    e.printStackTrace();
                }
                catch (TransformerFactoryConfigurationError e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
                catch (TransformerException e) {
                    response.sendError(500, e.getMessageAndLocation());
                    e.printStackTrace();
                }
                catch (IOException e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
                catch (DocumentException e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
            }
        }
        else {
            throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "You must include either parameter index or url to address the debug document to show", null);
        }
    }

    private void showTMLTags(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws HttpErrorException, IOException {
        String urlStr = request.getParameter("url");
        String indexStr = request.getParameter("index");

        if (urlStr != null) {
            urlStr = _dispatcher.getCore().getURLEncoder().decode(urlStr);
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            Document debugDoc;
            for (int idx = 0; idx < debugDocuments.size(); idx++) {
                debugDoc = (Document) debugDocuments.get(idx);
                if (debugDoc.getRootElement().attributeValue("url", "").equals(urlStr)) {
                    indexStr = String.valueOf(idx);
                    break;
                }
            }
        }

        response.setContentType("text/html");

        if (indexStr != null) {
            int index = Integer.valueOf(indexStr).intValue();
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            if (index == -1) {
                index = debugDocuments.size() - 1;
            }
            if (index >= debugDocuments.size()) {
                throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "Index out of range: " + index + " where maximum index is " + (debugDocuments.size() - 1), null);
            }
            else {
                Document doc = (Document) debugDocuments.get(index);
                Element element = (Element) doc.selectSingleNode(request.getParameter("root"));
                doc = DocumentFactory.getInstance().createDocument(element.createCopy());
                doc.getRootElement().addAttribute("index", String.valueOf(index));

                try {
                    DOMWriter domWriter = new DOMWriter();
                    org.w3c.dom.Document domDocument = domWriter.write(doc);

                    Transformer trans = getDebugTagsTransformer(request.getParameter("throwAway") != null);
                    trans.transform(new DOMSource(domDocument), new StreamResult(response.getOutputStream()));
                }
                catch (TransformerConfigurationException e) {
                    response.sendError(500, e.getMessageAndLocation());
                    e.printStackTrace();
                }
                catch (TransformerFactoryConfigurationError e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
                catch (TransformerException e) {
                    response.sendError(500, e.getMessageAndLocation());
                    e.printStackTrace();
                }
                catch (IOException e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
                catch (DocumentException e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
            }
        }
        else {
            throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "You must include either parameter index or url to address the debug document to show", null);
        }
    }

    private void showTMLPortlets(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws HttpErrorException, IOException {
        String urlStr = request.getParameter("url");
        String indexStr = request.getParameter("index");
    
        if (urlStr != null) {
            urlStr = _dispatcher.getCore().getURLEncoder().decode(urlStr);
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            Document debugDoc;
            for (int idx = 0; idx < debugDocuments.size(); idx++) {
                debugDoc = (Document) debugDocuments.get(idx);
                if (debugDoc.getRootElement().attributeValue("url", "").equals(urlStr)) {
                    indexStr = String.valueOf(idx);
                    break;
                }
            }
        }
    
        response.setContentType("text/html");
    
        if (indexStr != null) {
            int index = Integer.valueOf(indexStr).intValue();
            List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
            if (index == -1) {
                index = debugDocuments.size() - 1;
            }
            if (index >= debugDocuments.size()) {
                throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "Index out of range: " + index + " where maximum index is " + (debugDocuments.size() - 1), null);
            }
            else {
                Document doc = (Document) debugDocuments.get(index);
                doc.getRootElement().addAttribute("index", String.valueOf(index));
    
                try {
                    DOMWriter domWriter = new DOMWriter();
                    org.w3c.dom.Document domDocument = domWriter.write(doc);
    
                    Transformer trans = getDebugPortletsTransformer(request.getParameter("throwAway") != null);
                    trans.transform(new DOMSource(domDocument), new StreamResult(response.getOutputStream()));
                }
                catch (TransformerConfigurationException e) {
                    response.sendError(500, e.getMessageAndLocation());
                    e.printStackTrace();
                }
                catch (TransformerFactoryConfigurationError e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
                catch (TransformerException e) {
                    response.sendError(500, e.getMessageAndLocation());
                    e.printStackTrace();
                }
                catch (IOException e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
                catch (DocumentException e) {
                    response.sendError(500, e.getMessage());
                    e.printStackTrace();
                }
            }
        }
        else {
            throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, "You must include either parameter index or url to address the debug document to show", null);
        }
    }

}