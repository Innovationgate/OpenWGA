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
import java.net.URLDecoder;
import java.text.ParseException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.httpclient.util.URIUtil;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextAwareFormatter;

/**
 * Special output encoder that does all formattings neccessary for putting out
 * WGA Rich Text Fields:
 * <ul>
 * <li>Resolve Scriptlets
 * <li>Replace old RTF links with dynamic links
 * </ul>
 */
public class RTFEncodingFormatter implements TMLContextAwareFormatter, ReplaceProcessor {
    
   
    private Map _scriptletEngineParameters = new HashMap();

    private boolean _editMode;
    
    public static final String FLAG_ONLY_SYSTEM_MACROS = "onlysystemmacros";
    public static final String FLAG_IMG_DATA_URL = "imgdataurl";
    
    public static final String SYSPROP_FORCE_HTML_CLEANUP = "de.innovationgate.wga.rtf.forceHtmlCleanup";
    
    /**
     * Constructor
     * @param editMode Tells if this formatter generates the code to be displayed in RTF editor.
     */
    public RTFEncodingFormatter(boolean editMode) {
        _editMode = editMode;
    	Set flags = new HashSet();
    	if (editMode) {
    		flags.add(FLAG_ONLY_SYSTEM_MACROS);
    	}
    	configure(flags);
    }
    
    public RTFEncodingFormatter() {
        this(false);
    }
    
    public RTFEncodingFormatter(Set flags) {    	
    	configure(flags);
    }
    
	private void configure(Set flags) {
		if (flags != null) {
			if (flags.contains(FLAG_ONLY_SYSTEM_MACROS)) {
				_scriptletEngineParameters.put(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL, RhinoExpressionEngine.LEVEL_SYSTEM_MACROS);
			} else {
				_scriptletEngineParameters.put(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL, RhinoExpressionEngine.LEVEL_MACROS);
			}
			if (flags.contains(FLAG_IMG_DATA_URL)) {
				_scriptletEngineParameters.put(RhinoExpressionEngine.SCRIPTLETOPTION_IMAGEURL_AS_DATAURL, Boolean.TRUE);
			}
		}
	}

	private TMLContext _context;

    private Boolean _generateDataURL;

	/* (Kein Javadoc)
	 * @see de.innovationgate.utils.ObjectFormatter#format(java.lang.Object)
	 */
	public String format(Object obj) throws FormattingException {
		
		try {

		    String text = (String) obj;
		    _generateDataURL = (Boolean) _scriptletEngineParameters.get(RhinoExpressionEngine.SCRIPTLETOPTION_IMAGEURL_AS_DATAURL);
		    if (_generateDataURL == null) {
	            _generateDataURL = Boolean.FALSE;
	        }
		    
		    // Step one: Resolve scriptlets
		    RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();  
            text = engine.resolveScriptlets(text, _context, _scriptletEngineParameters);
            
            // Step two: Filter out all old relative links and replace them with dynamic URLs
            // We can ONLY do this in pure display mode. If field gets rendered for RTF editor we must prevent this because
            // it would lead to the storage of those dynamic URLs
            if (!_editMode) {
            	// replace qualified and unqualified attachment links
                text = WGUtils.strReplace(text, "=\"../", this, true);
                // replace content links
                text = replaceContentLinks(text);
            }
            
            if (_editMode && WGUtils.stringToBoolean(System.getProperty(SYSPROP_FORCE_HTML_CLEANUP, "false"))) {
	            // remove RTF-Editor declarations if contained in item value (possible due to copy&paste operations in the CM)
	            Pattern rtfEditorDeclarationPatter = Pattern.compile("(<span[^>]*class=\"WGA-Item[^\"]*[^>]*>)", Pattern.DOTALL);
	            Matcher matcher = rtfEditorDeclarationPatter.matcher(text);
	            while (matcher.find()) {
	            	String spanTag = matcher.group();
	            	if (spanTag.contains("display:none")) {
	            		text = matcher.replaceFirst("<span style=\"display:none\">");
	            	} else {
	            		text = matcher.replaceFirst("<span>");
	            	}
	            	matcher.reset(text);
	            }	
	            while(!removeNoOpSpanTags(text).equals(text)) {
	            	text = removeNoOpSpanTags(text);
	            }
            }
            
            return text;
            
        } 
        catch (WGException e) {
            throw new FormattingException("Exception on RTF-Encoding", e);
        }
	}
	
	private static String removeNoOpSpanTags(String input) {
		String[] tokens = input.split("</span>");
		String result = "";
		int count = 0;
		for (String token : tokens) {
			count++;
			if (token.lastIndexOf("<span>") != -1) {
				String prefix = token.substring(0, token.lastIndexOf("<span>"));        	
	        	String suffix = token.substring(token.lastIndexOf("<span>") + "<span>".length());
	        	if (!result.endsWith(" ") && !result.endsWith(">")) {
	        		result += " ";
	        	}
	        	result += prefix + suffix;
			} else if (count < tokens.length) {
				result += token + "</span>";
			} else {
				result += token;
			}			
		}		
		return result;
	}

	private String replaceContentLinks(String text) {
		String unmodifiedText = text;
		
		String wgaKeyPattern = "wgakey=\"";
		
		// search for 'wgakey="'
		int from = text.indexOf(wgaKeyPattern);
		while (from != -1) {
			try {
				int tagStart = text.lastIndexOf("<", from);
				int tagEnd = text.indexOf(">", from);
				
				String tagText = text.substring(tagStart, tagEnd);
				// wga key attribute found - check if this is not an external url link
				// some cm or bi versions might have generate wgakey if link type was "exturl"
				if (tagText != null && tagText.toLowerCase().indexOf("linktype=\"exturl\"") != -1) {
					// ignore this link - it should be external and wgakey is misleading here
					from = text.indexOf(wgaKeyPattern, from + wgaKeyPattern.length());
					continue;
				}
				
				
				// determine wgakey for replacement
				int wgakeyAttributeEnd = text.indexOf('"', from + wgaKeyPattern.length());
				String wgakey = text.substring(from + wgaKeyPattern.length(), wgakeyAttributeEnd);
				
				TMLContext targetContext = _context.context("docid:" + wgakey, false);
				if (targetContext != null) {														
					if (tagStart != -1 && tagEnd != -1) {
						String hrefPattern = "href=\"";
						int hrefStart = text.indexOf(hrefPattern, from);
						if (hrefStart != -1 && hrefStart < tagEnd) {
							// href attribute found behind wgakey-attribute
							int hrefEnd = text.indexOf("\"", hrefStart + hrefPattern.length());
							text = text.substring(0, hrefStart + hrefPattern.length()) + targetContext.contenturl(null, null) + text.substring(hrefEnd);
						} else {
							hrefStart = text.lastIndexOf(hrefPattern, from);
							if (hrefStart != -1 && hrefStart > tagStart) {
								// href attribute found before wgakey-attribute
								int hrefEnd = text.indexOf("\"", hrefStart + hrefPattern.length());
								text = text.substring(0, hrefStart + hrefPattern.length()) + targetContext.contenturl(null, null) + text.substring(hrefEnd);								
							}
						}
					}
				}	
			} catch (Exception e) {
				_context.getlog().error("Error parsing RTF field for old style content links", e);
				return unmodifiedText;
			}
			from = text.indexOf(wgaKeyPattern, from + wgaKeyPattern.length());							
		}
		return text;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.webtml.utils.TMLContextAwareFormatter#setContext(de.innovationgate.wgpublisher.webtml.utils.TMLContext)
	 */
	public void setContext(TMLContext context) {
		_context = context;
	}

    public int replace(String text, int from, int to, Writer out) throws IOException {
        
        // Default replacement if nothing else matches (or an error occurs): Put out the path unmodified;
        String replacement = text.substring(from, to);
        int returnIndex = to;

        try {
            // Check if we have the right type of attribute
            int attStart = text.lastIndexOf(" ", from);
            String attributeName = text.substring(attStart + 1, from);
            if (attributeName.equals("href") || attributeName.equals("src")) {
                
                // Get the remaining path
                int attEnd = text.indexOf("\"", to);
                String remainingPath = text.substring(to, attEnd);
                List tokens = WGUtils.deserializeCollection(remainingPath, "/");
                
                // Divide up operation by the beginning of this path
                
                // Content link, f.E.: default/owee-5aegzr
                /*
                if (remainingPath.startsWith("default/") && tokens.size() == 2) {
                    String structKey = (String) tokens.get(1);
                    TMLContext targetContext = _context.context("docid:" + structKey, false);
                    if (targetContext != null) {
                        replacement = "=\"" + targetContext.contenturl(null, null);
                        returnIndex = attEnd;
                    }
                }*/
                
                // Attachment link, unqualified db, f.E.: ../file/background/onlinedemo_bg.jpg
                if (remainingPath.startsWith("../file/") && tokens.size() == 4) {
                    String container = _context.getwgacore().getURLEncoder().decode((String) tokens.get(2));
                    String file = _context.getwgacore().getURLEncoder().decode((String) tokens.get(3));
                    if (attributeName.equals("src") && _generateDataURL.booleanValue() == true) {
                        replacement = "=\"" + _context.filedataurl(container, file);
                    }
                    else {
                        replacement = "=\"" + _context.fileurl(container, file);
                    }
                    returnIndex = attEnd;
                }
                
                // Attachment link, qualified db, f.E.: ../../mysql/file/images/uf005516.gif
                else if (remainingPath.startsWith("../../") && tokens.size() == 6 && tokens.get(3).equals("file")) {
                    String dbKey = (String) tokens.get(2);
                    String container = _context.getwgacore().getURLEncoder().decode((String) tokens.get(4));
                    String file = _context.getwgacore().getURLEncoder().decode((String) tokens.get(5));
                    if (attributeName.equals("src") && _generateDataURL.booleanValue() == true) {
                        replacement = "=\"" + _context.filedataurl(dbKey, container, file);
                    }
                    else {
                        replacement = "=\"" + _context.fileurl(dbKey, container, file);
                    }
                    returnIndex = attEnd;
                }
                
            }
        }
        catch (Exception e) {
            _context.getlog().error("Error parsing RTF field for old style relative links", e);
        }
        
        out.write(replacement);
        return returnIndex;
       
        
        
    }
}
