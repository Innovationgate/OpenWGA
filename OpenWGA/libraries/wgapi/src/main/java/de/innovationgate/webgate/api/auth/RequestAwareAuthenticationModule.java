package de.innovationgate.webgate.api.auth;

import javax.servlet.http.HttpServletRequest;

public abstract class RequestAwareAuthenticationModule implements AnonymousAwareAuthenticationModule{
	
	public abstract AuthenticationSession login(String username, Object credentials, HttpServletRequest request) throws AuthenticationException;
	
	public AuthenticationSession login(String username, Object credentials) throws AuthenticationException{
		return login(username, credentials, null);
	}
	
	public AuthenticationSession anonymousLogin(HttpServletRequest request) throws AuthenticationException {
		return AnonymousAuthSession.getInstance();
	}
}