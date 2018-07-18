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

package de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.cc;

import java.io.File;
import java.io.InputStream;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.servlet.http.Cookie;

import org.apache.commons.httpclient.HttpClient;

import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.Lucene;
import de.innovationgate.wga.server.api.Plugin;
import de.innovationgate.wga.server.api.Validate;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.Master;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.Xml;
import de.innovationgate.wgpublisher.mail.SmtpMail;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wga.server.api.Call;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.Session;

@CodeCompletion(propertyMode=CodeCompletion.MODE_EXCLUDE)
public abstract class WGAGlobal extends WGA {
    
    // Just so the class may compile
    public WGAGlobal() {
        super(null);
    }
    
    public Xml Xml = null;
    public de.innovationgate.wga.server.api.Html Html = null;
    public Master Master = null;
    public WGACore Core = null;
    public WGUtils Utils = null;
    public de.innovationgate.wga.server.api.Server Server = null;
    public de.innovationgate.wga.server.api.Jobs Jobs = null;
    public Validate Validate = null;

    public TMLPage TMLPage = null;
    public TMLScript TMLScript = null;
    public Call Call = null;
    public ObjectScope Scopes = null;
    public Session Session = null;
    public de.innovationgate.wga.server.api.Cookie Cookie = null;

    public abstract List<String> buildOptions(Iterable<WGContent> contents);
    public abstract List<String> buildOptions(Iterable<WGContent> contents, String titleExpression);
    public abstract List<String> buildOptions(Iterable<WGContent> contents, String titleExpression, String emptyTitle);
    
    public abstract Object callAction(String actionId, Object... params);
    public abstract Object callAction(TMLContext context, String actionId, Object... params);
    
    public abstract Calendar createCalendar(Date date);
    public abstract Calendar createCalendar();
    
    public abstract Cookie createCookie(String name, String value);
    
    public abstract Date createDate();
    public abstract Date createDate(boolean includingMillis);
    
    public abstract TMLForm createForm(TMLFormInfo formInfo);
    public abstract TMLFormInfo createFormInfo(String formId);
    
    public abstract ImageScaler createImageScaler(File imageFile);
    public abstract ImageScaler createImageScaler(InputStream imageData);
    
    public abstract List<Object> createList();
    public abstract List<Object> createList(Object[] array);
    public abstract List<String> createList(String listString, String divider);
    
    public abstract Map<Object,Object> createLookupTable();
    
    public abstract SmtpMail createMail();
    public abstract SmtpMail createMail(String smtpHost, String userName, String password);
    

    public abstract Object createObject(Object objectDefinition, Object... params);
    public abstract Object createObject(String moduleName, Object... params);

    
    public abstract WGDatabase db(String dbKey);
    
    public abstract List<Object> deleteDoublets(List<Object> list);
    
    public abstract Object deserializeObject(String serialized);
    
    public abstract Design design();
    public abstract Design design(WGDatabase designDB);
    public abstract Design design(Object currentObject);
    public abstract Design design(String dbKey);
    
    public abstract String encode(String encoding, Object input);
    
    public abstract String format(Date date, String pattern);
    public abstract String format(Number number, String pattern);
    
    public abstract List<Object> getLookupKeys(Map<Object,Object> lookupTable);
    
    public abstract WGHierarchicalDatabase hdb(String dbKey);
    
    public abstract Object javaObject(Object jsObject);
    
    public abstract void logException(Throwable e);
    public abstract void logException(String msg, Throwable e);
    
    public abstract Lucene lucene();
    public abstract Lucene lucene(TMLContext context);
    
    public abstract Date parseDate(String dateString, String format);
    
    public abstract Number parseNumber(String numberString, String format);
    
    public abstract Plugin plugin(String pluginName);
    public abstract Plugin plugin(WGDatabase pluginDb);
    
    public abstract void redirectTo(String url);
    
    public abstract String scoped(String str);
    public abstract String scoped(String str, String scope);
    
    public abstract String serializeObject(Object obj);
    
    public abstract void sortList(List<Object> list);
    public abstract void sortList(List<Object> list, String sortMeta);
    public abstract void sortList(List<Object> list, Object sortFunction);
    
    public abstract Object synchronizedFunction(Object function);
    
    public abstract URLBuilder urlBuilder();
    public abstract URLBuilder urlBuilder(String url);
    public abstract URLBuilder urlBuilder(TMLContext cx, String url);
    
    public abstract HttpClient createHttpClient();

}

