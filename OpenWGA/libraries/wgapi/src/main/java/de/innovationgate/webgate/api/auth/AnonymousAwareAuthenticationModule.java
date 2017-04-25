package de.innovationgate.webgate.api.auth;

import javax.servlet.http.HttpServletRequest;

public interface AnonymousAwareAuthenticationModule{

	public AuthenticationSession anonymousLogin(HttpServletRequest request) throws AuthenticationException;

}
