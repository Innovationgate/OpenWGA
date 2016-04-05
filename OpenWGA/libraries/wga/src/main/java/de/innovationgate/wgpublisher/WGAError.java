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
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.webtml.utils.HttpErrorException;


@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public class WGAError {
    
    public class WrappedException extends Throwable {
        
        private WrappedException(Throwable t) {
            super(t);
        }

        public String getMessage() {
            return WGUtils.encodeHTML(getCause().getMessage());
        }

        public String getLocalizedMessage() {
            return WGUtils.encodeHTML(getCause().getLocalizedMessage());
        }

        public String toString() {
            return WGUtils.encodeHTML(getCause().toString());
        }
        
        
        
    }

	private String detailMessage = "(No message type)";
	private Throwable exception = new Exception("(Unable to determine error)");
	private String stackTrace = "(No stack trace information)";
	private String maybeResponsibleDesign;
	private boolean httpError = false;
	private String dbHint = null;


	private Object mainMessage;

	private Object subMessage;
    private int errorCode = 200;
    
    public WGAError() {
        
    }
	
	public WGAError(Throwable exception, String maybeResponsibleDesign, ServletRequest request) {
		
		if (exception == null) {
			this.mainMessage =  "The page you requested is currently not available";
			this.subMessage = "The cause might be maintenance operations or technical problems. Please try again later.";			
			return;
		}
		
		this.maybeResponsibleDesign = maybeResponsibleDesign;
			
		this.exception = exception;
		if (this.exception instanceof HttpErrorException) {
			HttpErrorException httpError = (HttpErrorException) this.exception;
			this.mainMessage = "An error occured while processing your request";
			this.subMessage = "Http Error "  + httpError.getCode() + " - " + httpError.getMessage();
			this.detailMessage = null;
			this.stackTrace = "(None)";
            this.errorCode = httpError.getCode();
            this.dbHint = httpError.getDbHint();
            this.httpError = true;
		}
		else {
			this.mainMessage =  "The page you requested is currently not available";
			this.subMessage = "The cause might be maintenance operations or technical problems. Please try again later.";
			this.detailMessage = exception.getClass().getName();
			this.detailMessage = this.detailMessage.substring(this.detailMessage.lastIndexOf(".") + 1) + ": " + exception.getMessage();
			StringWriter stackTraceWriter = new StringWriter();
			PrintWriter stackTracePw =  new PrintWriter(stackTraceWriter);
			this.exception.printStackTrace(stackTracePw);
			this.stackTrace = stackTraceWriter.toString();
			
			HttpErrorException httpExc = new HttpErrorException((HttpServletRequest) request);
			this.errorCode = httpExc.getCode();
			
		}
	}

	/**
	 * Gets the mainException
	 * @return Returns a Throwable
	 */
	@CodeCompletion
	public Throwable getException() {
		return new WrappedException(exception);
	}
	
	/**
	 * Gets the typeMessage
	 * @return Returns a String
	 */
	@Deprecated
	public String getDetailMessage() {
	    if (detailMessage != null) {
	        return WGUtils.encodeHTML(detailMessage);
	    }
	    else {
	        return null;
	    }
	}
	    
	
	@CodeCompletion
	public String getDetailMessageHTML() {
		String msg = WGUtils.encodeHTML(detailMessage);
		msg = WGUtils.strReplace(msg, " ", "&nbsp;", true);
		return msg;
	}
	
	public String getTechnicalInformation() {
		
		StringBuffer html = new StringBuffer();
		
		if (maybeResponsibleDesign != null) {
			html.append("<div class='msg'><label>Rendering design:</label><div>").append(this.maybeResponsibleDesign).append("</div></div>");
		}
		html.append("<div class='msg'><label>Exception message:</label><div>").append(WGUtils.encodeHTML(this.exception.getLocalizedMessage())).append("</div></div>");
		html.append("<br>StackTrace:<br/>" + WGUtils.encodeHTML(this.stackTrace) + "<br/>");
		
		return html.toString();
	}
	
	/**
	 * Returns the httpError.
	 * @return boolean
	 */
	@CodeCompletion
	public boolean isHttpError() {
		return httpError;
	}
	
	@Deprecated
	public int getHttpCode() {
		
		if (this.isHttpError()) {
			return ((HttpErrorException) this.exception).getCode();
		}
		else {
			return -1;
		}
		
		
	}

	/**
	 * Returns the detailMessage.
	 * @return Object
	 */
	@CodeCompletion
	public Object getSubMessage() {
		return WGUtils.encodeHTML(String.valueOf(subMessage));
	}

	/**
	 * Returns the mainMessage.
	 * @return Object
	 */
	@CodeCompletion
	public Object getMainMessage() {
		return WGUtils.encodeHTML(String.valueOf(mainMessage));
	}

	/**
	 * Returns the maybeResponsibleDesign.
	 * @return String
	 */
	@Deprecated
	public String getMaybeResponsibleDesign() {
		return maybeResponsibleDesign;
	}

	/**
	 * Returns the stackTrace.
	 * @return String
	 */
	@Deprecated
	public String getStackTrace() {
		return stackTrace;
	}
	
	/**
	 * Retursn the error stack trace already formatted for HTML output
	 */
	@CodeCompletion
	public String getStackTraceHTML() {
	    return WGUtils.encodeHTML(this.stackTrace);
	}

    /**
     * @param detailMessage The detailMessage to set.
     */
    public void setDetailMessage(String detailMessage) {
        this.detailMessage = detailMessage;
    }

    /**
     * @param exception The exception to set.
     */
    public void setException(Throwable exception) {
        this.exception = exception;
    }

    /**
     * @param httpError The httpError to set.
     */
    public void setHttpError(boolean httpError) {
        this.httpError = httpError;
    }

    /**
     * @param mainMessage The mainMessage to set.
     */
    public void setMainMessage(Object mainMessage) {
        this.mainMessage = mainMessage;
    }

    /**
     * @param maybeResponsibleDesign The maybeResponsibleDesign to set.
     */
    public void setMaybeResponsibleDesign(String maybeResponsibleDesign) {
        this.maybeResponsibleDesign = maybeResponsibleDesign;
    }

    /**
     * @param stackTrace The stackTrace to set.
     */
    public void setStackTrace(String stackTrace) {
        this.stackTrace = stackTrace;
    }

    /**
     * @param subMessage The subMessage to set.
     */
    public void setSubMessage(Object subMessage) {
        this.subMessage = subMessage;
    }

    /**
     * @return Returns the errorCode.
     */
    @CodeCompletion
    public int getErrorCode() {
        return errorCode;
    }

    /**
     * @param errorCode The errorCode to set.
     */
    public void setErrorCode(int errorCode) {
        this.errorCode = errorCode;
    }
    
    @Deprecated
    public String getDbHint() {
        return this.dbHint;
    }
    
    @CodeCompletion
    public String getCausingDatabase() {
        return this.dbHint;
    }
    
    @CodeCompletion
    public String getCausingLayout() {
        return this.maybeResponsibleDesign;
    }
    

}

