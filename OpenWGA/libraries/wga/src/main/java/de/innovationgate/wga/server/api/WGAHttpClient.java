package de.innovationgate.wga.server.api;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpMethodBase;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.PutMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.commons.httpclient.methods.DeleteMethod;

import de.innovationgate.utils.Base64;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.modules.options.PasswordEncodingException;

@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class WGAHttpClient {

	private static String DEFAULT_CONTENTTYPE = "text/plain";
	
	public class Result{
		
		HttpMethod _method;
		int _status;
		
		public Result(int status, HttpMethod m){
			_status = status;
			_method = m;
		}
		
		public int getStatus(){
			return _status;
		}
		public String getStatusText(){
			return _method.getStatusText();
		}
		public String getText() throws IOException{
			return _method.getResponseBodyAsString();
		}
		public String getResponseBodyAsString(int limit) throws IOException{
			if(_method instanceof HttpMethodBase)
				return ((HttpMethodBase)_method).getResponseBodyAsString(limit);
			else return getText();
		}
		public InputStream getInputStream() throws IOException{
			return _method.getResponseBodyAsStream();
		}
		public String getHeader(String headerName){
			Header header = _method.getResponseHeader(headerName);
			return header == null ? null : header.getValue();
		}
		public String getContentType(){
			return getHeader("content-type");
		}
		public HttpMethod getMethod(){
			return _method;
		}
	}
	
	public interface Callback{
		public void call(Result result);
	}
	
	HttpClient _client;
	String _url;
	HashMap<String,String> _headers = new HashMap<String,String>();
	String _default_charset;
	
	public WGAHttpClient(WGA wga, String url) throws WGException {
		_client = WGFactory.getHttpClientFactory().createHttpClient();
		_url = url;
		_default_charset = wga.getCore().getCharacterEncoding();
	}
	
	public WGAHttpClient setRequestHeader(String header, String value){
		_headers.put(header, value);
		return this;
	}
	public WGAHttpClient setRequestHeaders(Map<String,String> map){
		_headers.putAll(map);
		return this;
	}
	
	public WGAHttpClient setDefaultCharset(String charset){
		_default_charset = charset;
		return this;
	}

	public WGAHttpClient setCredentials(String username, String password) throws PasswordEncodingException{
		String authString = (new Base64()).encodePassword(username + ":" + password);
		return setRequestHeader("Authorization", "Basic " + authString);
	}
	
	private int executeMethod(HttpMethod method, Callback callback) throws HttpException, IOException{
		// set request headers
		for(Entry<String,String> entry: _headers.entrySet()){
			method.setRequestHeader(entry.getKey(), entry.getValue());
		}
		// set default charset to be used if no charset is defined in response content-type
		HttpMethodParams params = new HttpMethodParams();
		params.setContentCharset(_default_charset);
		method.setParams(params);
		// execute
		try{
			int status = _client.executeMethod(method);
			if(callback!=null)
				callback.call(new Result(status, method));
			return status;
		}
		finally{
			method.releaseConnection();
		}
	}
	
	/*
	 * http-method get
	 */
	public int get(Callback callback) throws HttpException, IOException{
		return executeMethod(new GetMethod(_url), callback);
	}
	public int get() throws HttpException, IOException{
		return get(null);
	}

	/*
	 * http-method delete
	 */
	public int delete(Callback callback) throws HttpException, IOException{
		return executeMethod(new DeleteMethod(_url), callback);
	}
	public int delete() throws HttpException, IOException{
		return delete(null);
	}
	
	/*
	 * http-method post
	 */
	public int post(String body, String contentType, String charset, Callback callback) throws HttpException, IOException{
		PostMethod method = new PostMethod(_url);
		StringRequestEntity entity = new StringRequestEntity(body, contentType, charset);
		method.setRequestEntity(entity);
		return executeMethod(method, callback);
	}
	public int post(String body, String contentType, String charset) throws HttpException, IOException{
		return post(body, contentType, charset, null);
	}
	public int post(String body, String contentType, Callback callback) throws HttpException, IOException{
		return post(body, contentType, _default_charset, callback);
	}
	public int post(String body, String contentType) throws HttpException, IOException{
		return post(body, contentType, _default_charset, null);
	}
	public int post(String body, Callback callback) throws HttpException, IOException{
		return post(body, DEFAULT_CONTENTTYPE, _default_charset, callback);
	}
	public int post(String body) throws HttpException, IOException{
		return post(body, DEFAULT_CONTENTTYPE, _default_charset, null);
	}
	
	/*
	 * http-method put
	 */
	public int put(String body, String contentType, String charset, Callback callback) throws HttpException, IOException{
		PutMethod method = new PutMethod(_url);
		StringRequestEntity entity = new StringRequestEntity(body, contentType, charset);
		method.setRequestEntity(entity);
		return executeMethod(method, callback);
	}
	public int put(String body, String contentType, String charset) throws HttpException, IOException{
		return put(body, contentType, charset, null);
	}
	public int put(String body, String contentType, Callback callback) throws HttpException, IOException{
		return put(body, contentType, _default_charset, callback);
	}
	public int put(String body, String contentType) throws HttpException, IOException{
		return put(body, contentType, _default_charset, null);
	}
	public int put(String body, Callback callback) throws HttpException, IOException{
		return put(body, DEFAULT_CONTENTTYPE, _default_charset, callback);
	}
	public int put(String body) throws HttpException, IOException{
		return put(body, DEFAULT_CONTENTTYPE, _default_charset, null);
	}
	
}
