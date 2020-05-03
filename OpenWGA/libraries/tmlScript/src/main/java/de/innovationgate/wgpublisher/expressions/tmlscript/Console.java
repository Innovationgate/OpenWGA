package de.innovationgate.wgpublisher.expressions.tmlscript;

import java.util.HashMap;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class Console {
	
	private HashMap<String, Long> _timer= new HashMap<String, Long>();
    private Logger _logger;
    
	public Console(Logger logger) {
		_logger = logger;
	}
	
	public void setLogger(Logger logger){
		_logger = logger;
	}
	public Logger getLogger(){
		return _logger;
	}
	
	private String replaceVariables(String str0, Object[] obj) {
		String all= "";
		int index;
		int	counter= 0;
		
		// Workaround for an error when calling vararg methods from Rhino: A single null in varargs comes as a null array. Convert to an array containing a single null (#00003284):
		if (obj == null) {
		    obj = new Object[] { null };
		}
		
	    if (str0 == null) {
	        str0 = "null";
	    }
		
		if(str0.contains("%")) {
			for(int i= 0; i < obj.length && str0.contains("%"); ++i) {
				index= str0.indexOf("%");
				all+= str0.substring(0, index);
				str0= str0.substring(index);
				
				if(str0.charAt(1) == 's') {
					all += obj[i];
					str0= str0.substring(2);
					++counter;
				}
				
				else if(str0.charAt(1) == 'i' || str0.charAt(1) == 'd') {
					
				    try {
						all += ((Double)obj[i]).intValue();
					}
					catch(Exception e) {
						all += "NaN";
					}
				    
					str0= str0.substring(2);
					++counter;
				}
				
				else if(str0.charAt(1) == 'f') {
				    
					try {
						all += (Double)obj[i];
					}
					catch(Exception e) {
						all += "NaN";
					}
					str0= str0.substring(2);
					++counter;
				}
				
				else {
					all += "%";
					str0= str0.substring(1);
					--i;
				}
			}
		}
		
		all += str0;
		for( ; counter < obj.length; ++counter){
			all += " " + String.valueOf(obj[counter]);
	    }
		
		
		return all;
	}
	
	public void log(String str0, Object... obj) {
	    _logger.log(Level.INFO, replaceVariables(str0, obj));
	}
	
	public void debug(String str0, Object... obj) {
	    _logger.log(Level.DEBUG, replaceVariables(str0, obj));
	}
	
	public void info(String str0, Object... obj) {
	    _logger.log(Level.INFO, replaceVariables(str0, obj));
	}
	
	public void warn(String str0, Object... obj) {
	    _logger.log(Level.WARN, replaceVariables(str0, obj));
	}
	
	public void error(String str0, Object... obj) {
	    _logger.log(Level.ERROR, replaceVariables(str0, obj));
	}
	
	public void time(String str) {
		long starttime= System.currentTimeMillis();
		_logger.log(Level.INFO, str + ": timer started");
		_timer.put(str, starttime);
	}
	
	public void timeEnd(String str) {
		if(_timer.containsKey(str)) {
			long endtime= System.currentTimeMillis() - _timer.get(str);
			_logger.log(Level.INFO, str + ": " + endtime + "ms");
		} else {
		    _logger.log(Level.INFO, "Timer '" + str + "' not found.");
		}
	}
	
}
