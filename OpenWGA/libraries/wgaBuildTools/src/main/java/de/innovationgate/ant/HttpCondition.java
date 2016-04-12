package de.innovationgate.ant;

import java.io.IOException;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.condition.Condition;

/**
 * @author ow
 *
 * Folgendes auswählen, um die Schablone für den erstellten Typenkommentar zu ändern:
 * Fenster&gt;Benutzervorgaben&gt;Java&gt;Codegenerierung&gt;Code und Kommentare
 */
public class HttpCondition implements Condition {

	private String url = null;
	private String timeout = null;


	public boolean eval() throws BuildException {
		
		try {
			HttpClient client = new HttpClient();
			client.setConnectionTimeout((timeout == null ? 5000 : Integer.parseInt(timeout)));
			HttpMethod method = new GetMethod(url);
			method.setFollowRedirects(true);
			method.setStrictMode(false);
			client.executeMethod(method);
			return true;
		}
		catch (HttpException e) {
			return false;
		}
		catch (IOException e) {
			return false;
		}
		catch (NumberFormatException e) {
			throw new BuildException("Invalid timeout: " + timeout);
		}
		
	}	/**
	 * @param string
	 */
	public void setTimeout(String string) {
		timeout = string;
	}

	/**
	 * @param string
	 */
	public void setUrl(String string) {
		url = string;
	}

}
