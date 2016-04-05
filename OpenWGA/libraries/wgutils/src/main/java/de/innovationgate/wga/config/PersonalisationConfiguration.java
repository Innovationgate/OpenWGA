/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.config;

import java.util.ArrayList;
import java.util.List;

import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

/**
 * Global Configuration of OpenWGA personalisation 
 */
@Root(strict=false)
public class PersonalisationConfiguration extends ConfigBean {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;


    public static final String[] DEFAULT_EXCLUSIONS = new String[] {
        "Bloglines.*",
        "BlogUpdates.*", 
        "FAST-WebCrawler.*",
        "Feedreader.*",
        ".*Googlebot.*",
        "IPCHECK.*",
        ".*ZyBorg.*",
        ".*Yahoo! Slurp.*",
        ".*Slurp/cat.*",
        "msnbot.*",
        "NetNewsWire.*",
        "NewzCrawler.*",
        "NPBot.*",
        "SharpReader.*",
        "TurnitinBot.*",
        ".*nagios-plugins .*",
        "TwitterFeed .*",
        ".*bingbot/.*",
        "Feedfetcher-Google;.*",
        "Baiduspider+.*",
        ".*YandexBot/.*",
        ".*archive.org_bot .*",
        ".*DotBot/.*",
        "Sosospider+.*",
        "Sogou web spider/.*",
        ".* MJ12bot/.*",
        ".*ScoutJet;.*",
        ".*Purebot/.*"
    };
    
	
	@ElementList(required=false)
	@NotNull
	private List<String> personalisationAgentExclusions = new ArrayList<String>();

	public void setPersonalisationAgentExclusions(List<String> personalisationAgentExclusions) {
		if (personalisationAgentExclusions == null) {
			this.personalisationAgentExclusions = new ArrayList<String>();
		} else {
			this.personalisationAgentExclusions = personalisationAgentExclusions;
		}
	}

	public List<String> getPersonalisationAgentExclusions() {
		return personalisationAgentExclusions;
	}
	
	public void addDefaultAgentExclusions() {
	    for (String defEx : DEFAULT_EXCLUSIONS) {
	        if (!this.personalisationAgentExclusions.contains(defEx)) {
	            this.personalisationAgentExclusions.add(defEx);
	        }
	    }
	}

}
