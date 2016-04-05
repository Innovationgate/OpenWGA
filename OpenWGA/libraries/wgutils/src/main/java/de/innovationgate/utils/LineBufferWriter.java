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

package de.innovationgate.utils;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

import org.apache.commons.collections.Buffer;
import org.apache.commons.collections.BufferUtils;
import org.apache.commons.collections.buffer.UnboundedFifoBuffer;

/**
 * A writer that writes to a {@link Buffer}
 *
 */
public class LineBufferWriter extends Writer {

    private Buffer _buffer = BufferUtils.synchronizedBuffer(new UnboundedFifoBuffer());
    private StringBuffer _currentLine = new StringBuffer();
    private boolean _countLinesOnly = false;
    private long _lineCount = 0;
    
    public void close() throws IOException {
        flush();
    }

    public void flush() throws IOException {
        closeCurrentLine();
    }

    public synchronized void write(char[] chars, int offset, int length) throws IOException {
        String str = new String(chars, offset, length);
        appendToList(str);
    }

    private void appendToList(String str) {
        
        List lines = WGUtils.deserializeCollection(str, "\n");
        _currentLine.append(str);
        if (lines.size() > 1) {
            closeCurrentLine();
            if (lines.size() > 2) {
                List subList = lines.subList(1, lines.size() - 1);
                if (!_countLinesOnly) {
                    _buffer.addAll(subList);
                }
                _lineCount+=subList.size();
            }
            _currentLine.append(lines.get(lines.size() - 1));
        }
        
    }

    private void closeCurrentLine() {
        if (!_countLinesOnly) {
            _buffer.add(_currentLine.toString());
        }
        _currentLine = new StringBuffer();  
        _lineCount++;
    }

    /**
     * Returns the buffer with the written content
     * @throws IOException
     */
    public Buffer getBuffer() throws IOException {
        flush();
        return _buffer;
    }

    /**
     * Returns if the buffer should only count lines but store no written content
     */
    public boolean isCountLinesOnly() {
        return _countLinesOnly;
    }

    /**
     * Sets if the buffer should only count lines but store no written content
     * Defaults to false. When true no content will be written to the buffer
     */
    public void setCountLinesOnly(boolean countLinesOnly) {
        _countLinesOnly = countLinesOnly;
    }


    
    /**
     * Returns the number of lines in the buffer
     */
    public long getLineCount() {
        return _lineCount;
    }

}
