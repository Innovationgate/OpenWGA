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
package de.innovationgate.wgpublisher.filter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import de.innovationgate.wga.config.FilterMapping;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.services.WGAWebServicesFilter;

public class WGAFilterChain implements FilterChain {
	
    private static class FilterInfo {
        private Filter _filter;
        private WGAFilterConfig _config;
        private int _index;
        
        public FilterInfo(Filter filter, WGAFilterConfig config, int index) {
            _filter = filter;
            _config = config;
            _index = index;
        }

        public Filter getFilter() {
            return _filter;
        }        
        public WGAFilterConfig getConfig() {
            return _config;
        }        
        public int getIndex() {
            return _index;
        }
        public void setIndex(int index) {
            _index = index;
        }
    }

    public static final int INDEX_WGAFAKESESSIONFILTER = 100;
    public static final int INDEX_WGASERVICESSOAPFILTER = 200;
    public static final int INDEX_WGAVIRTUALHOSTINGFILTER = 300;
    
    public static final int INDEX_RUNTIMEFILTERS = 900;
	
	private List<Filter> _filters = new LinkedList<Filter>();
	
	// hashmap containing a urlpattern list for each filter
	private Map<Filter,List<String>>_filterToURLPatterns = new HashMap<Filter,List<String>>();
	
	// the following patterns are blacklisted for filter match
	// if they are not explicit whitelisted by _whitelistURLPatterns
	private static List<String> _blacklistURLPatterns = new LinkedList<String>();
	static {
		_blacklistURLPatterns.add("/static*");
		_blacklistURLPatterns.add("/wgadmin*");		
		_blacklistURLPatterns.add("/admin*");		
		_blacklistURLPatterns.add("/login*");
		_blacklistURLPatterns.add("/logout*");
		_blacklistURLPatterns.add("/domainkey*");
		_blacklistURLPatterns.add("*.jsp");
		_blacklistURLPatterns.add("/plugin-management*");
		_blacklistURLPatterns.add("/start");
		_blacklistURLPatterns.add("/joblog*");
		_blacklistURLPatterns.add("/favicon.ico");
		_blacklistURLPatterns.add("/tmlform/*");
	}
	
	// the following patterns are explicit whitelisted from the blacklist above
	private static List<String> _whitelistURLPatterns = new LinkedList<String>();
	static {
		_whitelistURLPatterns.add("/plugin-management/html/approval:*");
	}


	public WGAFilterChain(WGACore core, ServletContext servletContext) {
	    
	    // Built-in filters
	    List<FilterInfo> filterInfos = new ArrayList<FilterInfo>();
	    
	    createFakeSessionFilter(core, filterInfos);
	    
	    // WGAServices filter
        createWGAServicesFilter(core, filterInfos);

        createVirtualHostingFilter(core, filterInfos);
	    
	    // Filters from registered filter mappings
		Iterator<WGAFilterConfig> filterConfigs = core.getFilterMappings().iterator();
		// create filter instances and call init method
		int runtimeFilterIndex = INDEX_RUNTIMEFILTERS;
		while (filterConfigs.hasNext()) {
			WGAFilterConfig config = filterConfigs.next();
			if (config.isEnabled()) {
				config.setServletContext(servletContext);
				try {
					Filter filter = (Filter) WGACore.getLibraryLoader().loadClass(config.getFilterClassName()).newInstance();
					// call init method
					filter.init(config);
					
					// create filter info
					FilterInfo info = null;
                    if (filter instanceof IndexProvidingServletFilter) {
                        info = new FilterInfo(filter, config, ((IndexProvidingServletFilter)filter).getIndex());
                    } else {
                        info = new FilterInfo(filter, config, runtimeFilterIndex++);
                    }
                    filterInfos.add(info);
					
				} catch (Throwable e) {
					core.getLog().error("Unable to initialize filter '" + config.getFilterName() + "' - '" + config.getFilterClassName() + "'.", e);
				}	
			}
		}		
		
		Collections.sort(filterInfos, new Comparator<FilterInfo>() {
            @Override
            public int compare(FilterInfo info1, FilterInfo info2) {
                return info1.getIndex() - info2.getIndex();
            }           

        });
        
        Iterator<FilterInfo> it = filterInfos.iterator();
        while (it.hasNext()) {
            FilterInfo info = it.next();
            _filters.add(info.getFilter());
            _filterToURLPatterns.put(info.getFilter(), info.getConfig().getUrlpatterns());
            core.getLog().info("Filter '" + info.getConfig().getFilterName() + "' added to filter chain - #" + info.getIndex());
        }
	}
	
	

	private void createWGAServicesFilter(WGACore core, List<FilterInfo> filterInfos) {
	    
        try {
            // Create and init the filter (which we do anyway, even if there are no protocol implementation)
            FilterMapping filterMapping = new FilterMapping();
            filterMapping.setName("WGAServices SOAP Request Filter");
            filterMapping.setEnabled(true);
            filterMapping.setImplClassName(WGAWebServicesFilter.class.getName());
            
            List<String> patterns = new ArrayList<String>();
            patterns.add(WGAWebServicesFilter.BASEPATH);
            patterns.add(WGAWebServicesFilter.BASEPATH + "/*");
            // patterns.add("//services/*"); // Really necessary? Breaks internal rewritings of the services filter
            filterMapping.setUrlPatterns(patterns);
            
            WGAWebServicesFilter filter = new WGAWebServicesFilter();
            WGAFilterConfig filterConfig = WGAFilterConfig.createFromMapping(filterMapping);
            filterConfig.setServletContext(core.getServletContext());
            filter.init(filterConfig);
            
            FilterInfo info = new FilterInfo(filter, filterConfig, INDEX_WGASERVICESSOAPFILTER);
            filterInfos.add(info);
        }
        catch (Exception e) {
            core.getLog().error("Exception initializing WGAServices SOAP protocol", e);
        }

    }

	   private void createVirtualHostingFilter(WGACore core, List<FilterInfo> filterInfos) {
	        
	        try {	            	            
	            FilterMapping filterMapping = new FilterMapping();
	            filterMapping.setName("WGA Virtual Hosting Filter");
	            filterMapping.setEnabled(true);
	            filterMapping.setImplClassName(WGAVirtualHostingFilter.class.getName());
	            
	            List<String> patterns = new ArrayList<String>();
	            patterns.add("*");
	            filterMapping.setUrlPatterns(patterns);
	            
	            Map<String, String> parameters = new HashMap<String, String>();
	            filterMapping.setInitParameters(parameters);
	            
	            WGAVirtualHostingFilter filter = new WGAVirtualHostingFilter();
	            WGAFilterConfig filterConfig = WGAFilterConfig.createFromMapping(filterMapping);
	            filterConfig.setServletContext(core.getServletContext());
	            filter.init(filterConfig);
	            FilterInfo info = new FilterInfo(filter, filterConfig, INDEX_WGAVIRTUALHOSTINGFILTER);
                filterInfos.add(info);                  	            
	        }
	        catch (Exception e) {
	            core.getLog().error("Exception initializing WGA Virtual Hosting Filter", e);
	        }

	    }

   private void createFakeSessionFilter(WGACore core, List<FilterInfo> filterInfos) {
       
       try {                               
           FilterMapping filterMapping = new FilterMapping();
           filterMapping.setName("WGA Fake Session Filter");
           filterMapping.setEnabled(true);
           filterMapping.setImplClassName(WGAFakeSessionFilter.class.getName());
           
           List<String> patterns = new ArrayList<String>();
           patterns.add("*");
           filterMapping.setUrlPatterns(patterns);
           
           Map<String, String> parameters = new HashMap<String, String>();
           filterMapping.setInitParameters(parameters);
           
           WGAFakeSessionFilter filter = new WGAFakeSessionFilter();
           WGAFilterConfig filterConfig = WGAFilterConfig.createFromMapping(filterMapping);
           filterConfig.setServletContext(core.getServletContext());
           filter.init(filterConfig);
           FilterInfo info = new FilterInfo(filter, filterConfig, INDEX_WGAFAKESESSIONFILTER);
           filterInfos.add(info);              
       }
       catch (Exception e) {
           core.getLog().error("Exception initializing WGA Fake Session Filter", e);
       }

   }	   

    public void doFilter(ServletRequest request, ServletResponse response)
			throws IOException, ServletException {
		
		Integer filterIndex = (Integer)request.getAttribute("de.innovationgate.wga.filter.index");
		if (filterIndex == null) {
			filterIndex = new Integer(0);
			request.setAttribute("de.innovationgate.wga.filter.index", filterIndex);
		}
		
		if (_filters.size() > filterIndex.intValue()) {
			// retrieve filter
			Filter filter = (Filter) _filters.get(filterIndex.intValue());
			
			filterIndex = new Integer(filterIndex.intValue() + 1);
			request.setAttribute("de.innovationgate.wga.filter.index", filterIndex);
			
			// check URLPattern
			if (callFilterWithinThisRequest(request, filter)) {
				// call filter
				filter.doFilter(request, response, this);				
			} else {
				// go to next filter
				doFilter(request, response);
			}
			
			
			
		} else {
			// last filter reached - dispatch to parent chain
			((FilterChain)request.getAttribute("de.innovationgate.wga.filter.parentChain")).doFilter(request, response);
		}
	}



	private boolean callFilterWithinThisRequest(ServletRequest request, Filter filter) {
		HttpServletRequest hreq = (HttpServletRequest) request;
		
		String url = hreq.getRequestURI().substring(hreq.getContextPath().length());
		
		// retrieve urlpatterns
		List<String> urlpatterns = _filterToURLPatterns.get(filter);
		
		if (urlpatterns == null || urlpatterns.isEmpty()) {
			return false;
		} else {
		    List<String> blackList = new ArrayList<String>();
		    blackList.addAll(_blacklistURLPatterns);
		    
		    List<String> whiteList = new ArrayList<String>();
            whiteList.addAll(_whitelistURLPatterns);
		    
            
            if (filter instanceof WGAFilterURLPatternProvider) {
                WGAFilterURLPatternProvider provider = (WGAFilterURLPatternProvider)filter;
                if (provider.getBlackListURLPatterns() != null) {
                    blackList.addAll(provider.getBlackListURLPatterns());
                }
                if (provider.getWhiteListURLPatterns() != null) {
                    whiteList.addAll(provider.getWhiteListURLPatterns());    
                }                
            }
            
			// first check black and whitelist
			// if not blacklisted check configured patterns
			if (matches(url, blackList) && !(matches(url, whiteList))) {
				return false;				
			} else {
				return matches(url, urlpatterns);
			}
		}		
	}
	
	private boolean matches(String url, List<String> patterns) {
		// first check if url match exactly
		if (patterns.contains(url)) {
			return true;
		} else {
			Iterator<String> patternsIt = patterns.iterator();
			while (patternsIt.hasNext()) {
				String pattern = patternsIt.next();
				if (pattern.startsWith("*")) {
					pattern = pattern.substring(pattern.indexOf("*")+1);
					if (url.endsWith(pattern)) {
						return true;
					}
				} else if (pattern.endsWith("*")) {
					pattern = pattern.substring(0, pattern.indexOf("*"));
					if (url.startsWith(pattern)) {
						return true;
					}
				}					
			}		
		}
		return false;
	}



	public void init(ServletRequest request, FilterChain parentChain) {
		request.setAttribute("de.innovationgate.wga.filter.parentChain", parentChain);
	}

    public void destroy() {
        for (Filter filter : _filters) {
            filter.destroy();
        }
    }

}
