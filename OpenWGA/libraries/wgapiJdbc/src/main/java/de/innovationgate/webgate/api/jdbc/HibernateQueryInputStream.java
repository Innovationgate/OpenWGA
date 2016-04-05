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
package de.innovationgate.webgate.api.jdbc;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Blob;
import java.sql.SQLException;
import java.util.Iterator;

import javax.imageio.IIOException;

import org.hibernate.Hibernate;
import org.hibernate.Query;
import org.hibernate.Session;

import de.innovationgate.utils.io.IOBackendException;

/**
 * Input stream implementation for Hibernate-Blob-Queries
 *
 */
public class HibernateQueryInputStream extends InputStream {
	
	private static final int DEFAULT_BUFFERSIZE = 32; // 32 * 64k --> 2M

	private Iterator _data;
	
	private InputStream _input;
	private AttachmentFilePart _part;
	
	private int _resultPos = 0;

	private Query _query;
	
	private int _bufferSize = 0;

	private boolean _disableQueryPaging;

    private Session _session;
    
    private boolean _eof = false;
	

	/**
	 * Creates an input stream from the given query
	 * The query should return only one column of type <code>Blob</code>
	 * @param query
	 * @param bufferSize the buffer size to use for streaming - actual heap usage will be (bufferSize * 64k)
	 * 					 if bufferSize is <= 0 the default 32 will be used
	 * @param disableQueryPaging true/false should hibernate query paging be used
	 * 					on databases like MySQL and Oracle query paging is recommend, on MSSQL paging will result in poor performance 
	 * 					and should be disabled 
	 */
	public HibernateQueryInputStream(Session session, Query query, int bufferSize, boolean disableQueryPaging) {
	    _session = session;
		_query = query; 
		_disableQueryPaging = disableQueryPaging;
		_bufferSize = bufferSize;
		if (_bufferSize <= 0) {
			_bufferSize = DEFAULT_BUFFERSIZE;
		}
		_query.setFetchSize(_bufferSize);
		if (!_disableQueryPaging) {
			_query.setFirstResult(0);
			_query.setMaxResults(_bufferSize);
			_resultPos = _bufferSize;
		}
		
		_data = _query.iterate();
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.InputStream#read()
	 */
	public int read() throws IOException {
		try {
		    if (_eof) {
		        return -1;
		    }
		    
			if (_input == null) {
				// try to fetch first blob of the file
				if (_data.hasNext()) {
				    _part = (AttachmentFilePart) _data.next();
					_input = _part.getData().getBinaryStream();
				} else {
				    _eof = true;
					return -1;
				}
			}
			
			int data = _input.read();			
			if (data != -1) {
				return data;
			}
			else {
			    _session.evict(_part);
			    if (_input != null) {
			        _input.close();
			    }
			    _part = fetchNextPart();
			    if (_part != null) {
			        _input = _part.getData().getBinaryStream();
			        data = _input.read();
			        if (data == -1) {
                        _input.close();
                    }
                    return data;
			    }
			    else {
			        _eof = true;
			        return -1;
			    }
			}
		}
		catch (SQLException e) {
			throw new HibernateQueryException("Exception reading file data", e); 
		}		
	}
	
	private AttachmentFilePart fetchNextPart() throws SQLException {
	    
	    if (_data.hasNext()) {
            // fetch next part of file
            return (AttachmentFilePart) _data.next();
        }
        
        else {
            if (_disableQueryPaging) {
                // end of file reached
                return null;
            } else {
                // try if we can read more results from the database
                _query.setFirstResult(_resultPos);
                _resultPos += _bufferSize;
                _data = _query.iterate();
                if (_data.hasNext()) {
                   return (AttachmentFilePart) _data.next();
  
                } else {
                    // no more parts available - end of stream reached
                    return null;
                }
            }
        }
	    
	}

    @Override
    public long skip(long n) throws IOException {
        
        long skipped = 0;
        
        try {
            // Skip remainder bytes of the current input
            if (_input != null) {
                long skippedNow = _input.skip(n);
                if (skippedNow > 0) {
                    skipped += skippedNow;
                }
                if (skipped >= n) {
                    return skipped;
                }
            }
            
            // Skip 64 kb blocks
            while ((n - skipped) >= WGDocumentImpl.ATTACHMENT_FILEPART_SIZE) {
                _part = fetchNextPart();
                if (_part == null) {
                    return skipped;
                }
                _session.evict(_part);
                skipped += WGDocumentImpl.ATTACHMENT_FILEPART_SIZE; // Is inexact since the last part may be <64 kb. But since we then have reached the end it should not matter what to report.
            }
            
            // Skip remaining bytes
            if ((n - skipped) > 0) {
                _part = fetchNextPart();
                if (_part != null) {
                    _input = _part.getData().getBinaryStream();
                    long skippedNow = _input.skip(n - skipped);
                    if (skippedNow > 0) {
                        skipped += skippedNow;
                    }
                }
            }
            return skipped;
        }
        catch (SQLException e) {
            throw new IOBackendException("Error executing SQL", e);
        }
        
    }
    
    @Override
    public void close() throws IOException {
        if (_part != null) {
            _session.evict(_part);
        }
        Hibernate.close(_data);
        super.close();
    }

}
