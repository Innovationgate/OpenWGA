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

package de.innovationgate.wgpublisher.cluster.tasks;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;

/**
 * retrieves problems of the given type
 * further a problem scope can be specified to filter problems
 * 
 * Note:
 * The problem beans used by the problem registry are not guaranteed to be serializable. 
 * Therefore this task return a wrapper bean 'ProblemDescription' and not the problem instance. 
 */
public class RetrieveProblemsTask extends ClusterTask<List<RetrieveProblemsTask.ProblemDescription>> {

    private static final long serialVersionUID = 1L;

    public static class ProblemDescription implements Serializable {
        private static final long serialVersionUID = 1L;
        private String _title;
        private String _description;
        private ProblemSeverity _severity;
        
        public String getTitle() {
            return _title;
        }
        public void setTitle(String title) {
            _title = title;
        }
        
        public String getDescription() {
            return _description;
        }
        public void setDescription(String description) {
            _description = description;
        }
        
        public void setSeverity(ProblemSeverity severity) {
            _severity = severity;            
        }
        public ProblemSeverity getSeverity() {
            return _severity;
        }

    }
    
    private Class<? extends ProblemType> _type;
    private ProblemScope _scope;
    private Locale _locale = Locale.ENGLISH;
    
    public RetrieveProblemsTask(Class<? extends ProblemType> type) {
        _type = type;
    }

    public Class<? extends ProblemType> getType() {
        return _type;
    }

    public void setType(Class<? extends ProblemType> type) {
        _type = type;
    }

    public ProblemScope getScope() {
        return _scope;
    }

    public void setScope(ProblemScope scope) {
        _scope = scope;
    }

    public Locale getLocale() {
        return _locale;
    }

    public void setLocale(Locale locale) {
        _locale = locale;
    }

    @Override
    public List<ProblemDescription> execute() throws Exception {
        List<ProblemDescription> problemDescs = new ArrayList<ProblemDescription>();
        WGACore core = getContext().getWGACore();
        Iterator<Problem> problems = null;
        if (_scope != null) {
            problems = core.getProblemRegistry().getProblems(_type, _scope).iterator();
        } else {
            problems = core.getProblemRegistry().getProblems(_type).iterator();
        }
        while (problems.hasNext()) {
            Problem problem = problems.next();
            ProblemDescription desc = new ProblemDescription();
            
            desc.setTitle(problem.getTitle(_locale));
            desc.setDescription(problem.getDescription(_locale));
            desc.setSeverity(problem.getSeverity());
            problemDescs.add(desc);
        }
        return problemDescs;
    }

}
