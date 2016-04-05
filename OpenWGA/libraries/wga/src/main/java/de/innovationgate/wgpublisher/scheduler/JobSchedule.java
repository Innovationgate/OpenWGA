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

package de.innovationgate.wgpublisher.scheduler;

import java.text.SimpleDateFormat;
import java.util.Date;

import de.innovationgate.wga.config.Schedule;

public class JobSchedule {

    public static final String TYPE_SIMPLE = Schedule.TYPE_SIMPLE;
    public static final String TYPE_CRON = Schedule.TYPE_CRON;
    
    private boolean _enabled;
    private String _type;
    private String _scheduleData;
    private Date _startingDate;
    private Date _endingDate;
    
    public JobSchedule() {
        
    }
    
    public JobSchedule(Schedule config) throws ConfigurationException {
        
        _enabled = config.isEnabled();
        
        _type = config.getType();
        _scheduleData = config.getData();
        _startingDate = config.getStartDate();
        _endingDate = config.getEndDate();

        
    }
    
    public boolean isEnabled() {
        return _enabled;
    }
    public void setEnabled(boolean enabled) {
        _enabled = enabled;
    }
    public Date getEndingDate() {
        return _endingDate;
    }
    public void setEndingDate(Date endingDate) {
        _endingDate = endingDate;
    }
    public String getScheduleData() {
        return _scheduleData;
    }
    public void setScheduleData(String scheduleData) {
        _scheduleData = scheduleData;
    }
    public Date getStartingDate() {
        return _startingDate;
    }
    public void setStartingDate(Date startingDate) {
        _startingDate = startingDate;
    }
    public String getType() {
        return _type;
    }
    public void setType(String type) {
        _type = type;
    }    
}
