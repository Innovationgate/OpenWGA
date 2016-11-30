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

package de.innovationgate.webgate.api;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectStreamException;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import de.innovationgate.utils.WGUtils;

/**
 * Represents a database revision and provides methods to operate with it, including comparision, serialisation and derserialisation
 */
public class WGDatabaseRevision implements Comparable<WGDatabaseRevision>, Serializable {

    private static final long serialVersionUID = 1L;

    private static final DateFormat DATE_SERIALISATION_FORMAT = new SimpleDateFormat("yyyyMMdd-HHmmss");
    
    private Date _revisionDate = null;
    private Long _revisionNumber = null;
    private Date _readDate = new Date();
    
    /**
     * Returns the raw revision value
     */
    public Comparable<?> getRevisionValue() {
        if (_revisionNumber != null) {
            return _revisionNumber;
        }
        else {
            return _revisionDate;
        }
    }

    public WGDatabaseRevision(Number revisionNumber) {
        if (revisionNumber == null) {
            throw new IllegalArgumentException("Illegal null revision");
        }
        _revisionNumber = revisionNumber.longValue();
    }
    
    protected WGDatabaseRevision(Date revisionDate) {
        if (revisionDate == null) {
            throw new IllegalArgumentException("Illegal null revision");
        }
        _revisionDate = revisionDate;
    }

    /**
     * Creates a database revision object from a raw revision value
     * @param o
     * @return The database revision object
     * @throws IOException 
     */
    public static WGDatabaseRevision forValue(Comparable<?> o) {
        
        WGDatabaseRevision otherRevision = null; 
        if (o instanceof WGDatabaseRevision) {
            otherRevision = (WGDatabaseRevision) o;
        }
        else if (o instanceof Date) {
            otherRevision = new WGDatabaseRevision((Date) o);
        }
        else if (o instanceof Number) {
            otherRevision = new WGDatabaseRevision((Number) o);
        }
        else if (otherRevision == null) {
            otherRevision = new WGDatabaseRevision(new Date(Long.MIN_VALUE));
        }
        return otherRevision;
    }
    
    /**
     * Deserializes a serialized revision
     * @param serialized The serialized revision
     * @throws IOException
     */
    public static WGDatabaseRevision deserialize(String serialized) throws IOException {
        Comparable<?> value = WGDatabaseRevision.readPersistentValue(serialized);
        return forValue(value);
    }
    
    /**
     * Return a persisteable serialized form of this revision
     * @throws IOException
     */
    public String serialize() throws IOException {
        return writePersistentValue();
    }

    private void writeObject(java.io.ObjectOutputStream out) throws IOException {
        OutputStreamWriter writer = new OutputStreamWriter(out, "UTF-8");
        writer.write(writePersistentValue());
        writer.flush();
    }
    
    private String writePersistentValue() {
        if (_revisionNumber != null) {
            return _revisionNumber.toString();
        }
        else {
            return DATE_SERIALISATION_FORMAT.format(_revisionDate);
        }
    }

    private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
        InputStreamReader reader = new InputStreamReader(in, "UTF-8");
        String str = WGUtils.readString(reader);
        Comparable<?> revisionValue = readPersistentValue(str);
        if (revisionValue instanceof Date) {
            _revisionDate = (Date) revisionValue;
        }
        else if (revisionValue instanceof Long) {
            _revisionNumber = (Long) revisionValue; 
        }
        else {
            throw new IOException("Invalid persistent value type: " + revisionValue.getClass().getName());
        }
    }
    
    protected static Comparable<?> readPersistentValue(String str) throws IOException {

        try {
            int linePos = str.indexOf("-");
            if (linePos == 8)  { // Serialized date
                return DATE_SERIALISATION_FORMAT.parse(str);
            }
            else {
                return Long.valueOf(str);
            }
        }
        catch (Exception e) {
            throw new IOException("Exception deserializing revision value '" + str + "'", e);
        }
        
        
    }

    @SuppressWarnings("unused")
    private void readObjectNoData() throws ObjectStreamException {
        _revisionDate = new Date(Long.MIN_VALUE);
    }
    
    @Override
    public int compareTo(WGDatabaseRevision otherRevision) {
        
        if (_revisionNumber != null && otherRevision._revisionNumber != null) {
            return _revisionNumber.compareTo(otherRevision._revisionNumber);
        }
        else if (_revisionDate != null && otherRevision._revisionDate != null) {
            return _revisionDate.compareTo(otherRevision._revisionDate);
        }
        else { // Revisions have differing revision types. The one with the number is considered more "modern" and therefor newer
            if (_revisionNumber == null) {
                return 1;
            }
            else {
                return -1;
            }
            
        }
        
    }

    /**
     * Returns if the current revision may be newer than the given revision.
     * This is only a probable value because with non-unique revisions (CS3) it is not determinable just by the revision values if one revision may be newer than the other.
     * They may have the same value yet still originate from different revisions.
     */
    public boolean isProbablyNewerThan(WGDatabaseRevision cacheRevision) {

        if (_revisionDate != null) {
            Comparable<?> cacheRevisionValue = cacheRevision.getRevisionValue();
            if (cacheRevisionValue instanceof Date) {
                Date cacheRevisionDate = (Date) cacheRevisionValue;
                if (_revisionDate.after(cacheRevisionDate)) {
                    return true;
                }
                else if (_revisionDate.equals(cacheRevisionDate)) { // Must be true in case multiple changes have the same revision date
                    return true;
                }
                else {
                    return false;
                }
            }
            else {
                return true;
            }
        }
        else {
            Comparable<?> cacheRevisionValue = cacheRevision.getRevisionValue();
            if (cacheRevisionValue instanceof Number) {
                Number cacheRevisionNumber = (Number) cacheRevisionValue;
                if (_revisionNumber.longValue() > cacheRevisionNumber.longValue()) {
                    return true;
                }
                else {
                    return false;
                }
            }
            else {
                return false;
            }
        }
    
    }
    
    /**
     * Returns if this revision value is unique among all revisions of the database. False means that there may be multiple revisions with this revision value.
     */
    public boolean isUniqueValue() {
        return (_revisionNumber != null);
    }

    /**
     * Checks for non-unique revisions if it is possible that a later revision with equal revision value exists than the revision from which this value was read.
     * To determine this the method uses the time when this revision value was read. If after this time it is impossible that another new revision uses the same revision value it returns false. 
     */
    public boolean isLaterEqualRevisionValuePossible() {
        if (_revisionDate != null) {
            long readTime = _readDate.getTime();
            long readTimeWithoutMillis = new Double(Math.floor((double) readTime / 1000) * 1000).longValue();
            Date readDateWithoutMillis = new Date(readTimeWithoutMillis);
            return !_revisionDate.before(readDateWithoutMillis);
        }
        else {
            return false;
        }
    }

    /**
     * Returns the time when this revision object was created, assuming this was also the time when the revision was read
     */
    protected Date getReadDate() {
        return _readDate;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_revisionDate == null) ? 0 : _revisionDate.hashCode());
        result = prime * result + ((_revisionNumber == null) ? 0 : _revisionNumber.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        WGDatabaseRevision other = (WGDatabaseRevision) obj;
        if (_revisionDate == null) {
            if (other._revisionDate != null)
                return false;
        }
        else if (!_revisionDate.equals(other._revisionDate))
            return false;
        if (_revisionNumber == null) {
            if (other._revisionNumber != null)
                return false;
        }
        else if (!_revisionNumber.equals(other._revisionNumber))
            return false;
        return true;
    }
    
    @Override
    public String toString() {
        try {
            return serialize();
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
