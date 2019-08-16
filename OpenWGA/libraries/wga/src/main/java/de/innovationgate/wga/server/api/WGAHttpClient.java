package de.innovationgate.wga.server.api;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.PutMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.methods.DeleteMethod;

import de.innovationgate.utils.Base64;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.modules.options.PasswordEncodingException;

public class WGAHttpClient {

	private static String DEFAULT_CHARSET = "UTF-8";
	
	public class Result{
		
		private int _status;
		private String _statusText;
		private String _body;
		
		Result(int status, String statusText, String body){
			_status = status;
			_statusText = statusText;
			_body = body;			
		}
		
		public int getStatus(){
			return _status;
		}
		public String getStatusText(){
			return _statusText;
		}
		public String getBody() throws IOException{
			return _body;
		}
	}
	
	HttpClient _client;
	String _url;
	HashMap<String,String> _headers = new HashMap<String,String>();
	
	public WGAHttpClient(String url) {
		_client = WGFactory.getHttpClientFactory().createHttpClient();
		_url = url;
	}
	
	public WGAHttpClient setRequestHeader(String header, String value){
		_headers.put(header, value);
		return this;
	}
	public WGAHttpClient setRequestHeaders(Map<String,String> map){
		_headers.putAll(map);
		return this;
	}

	public WGAHttpClient setCredentials(String username, String password) throws PasswordEncodingException{
		String authString = (new Base64()).encodePassword(username + ":" + password);
		return setRequestHeader("Authorization", "Basic " + authString);
	}
	
	private Result executeMethod(HttpMethod method) throws HttpException, IOException{
		// set request headers
		for(Entry<String,String> entry: _headers.entrySet()){
			method.setRequestHeader(entry.getKey(), entry.getValue());
		}
		// execute
		int status = _client.executeMethod(method);
		String statusText = method.getStatusText();
		String body = method.getResponseBodyAsString();
		method.releaseConnection();
		
		return new Result(status, statusText, body);
	}
	
	public HttpClient getClient(){
		return _client;
	}
	
	public Result get() throws HttpException, IOException{
		return executeMethod(new GetMethod(_url));
	}

	public Result delete() throws HttpException, IOException{
		return executeMethod(new DeleteMethod(_url));
	}

	public Result post(String body, String contentType, String charset) throws HttpException, IOException{
		PostMethod method = new PostMethod(_url);
		StringRequestEntity entity = new StringRequestEntity(body, contentType, charset);
		method.setRequestEntity(entity);
		return executeMethod(method);
	}
	public Result post(String body, String contentType) throws HttpException, IOException{
		return post(body, contentType, DEFAULT_CHARSET);
	}
	public Result post(String body) throws HttpException, IOException{
		return post(body, null, null);
	}
	

	public Result put(String body, String contentType, String charset) throws HttpException, IOException{
		PutMethod method = new PutMethod(_url);
		StringRequestEntity entity = new StringRequestEntity(body, contentType, charset);
		method.setRequestEntity(entity);
		return executeMethod(method);
	}
	public Result put(String body, String contentType) throws HttpException, IOException{
		return put(body, contentType, DEFAULT_CHARSET);
	}
	public Result put(String body) throws HttpException, IOException{
		return put(body, null, null);
	}
	
}
