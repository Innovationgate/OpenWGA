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

package de.innovationgate.wgpublisher.encoders;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.innovationgate.wgpublisher.encoders.TextChunk.Origin;

/**
 * Output object for WGA encoders
 */
public class EncoderOutput {
    
    private List<TextChunk> _outputChunks = new ArrayList<TextChunk>();
    private boolean _ignored= false;
    private TextChunk _inputChunk;
    private List<WGAEncoder> _chainedEncoders = new ArrayList<WGAEncoder>();
    
    /**
     * Returns encoders that were chained after the current encoder run
     */
    public List<WGAEncoder> getChainedEncoders() {
        return _chainedEncoders;
    }

    public EncoderOutput(TextChunk inputChunk) {
        _inputChunk = inputChunk;
    }
    
    /**
     * Add a new text chunk to the output
     * @param origin The origin of the chunk
     * @param contentType The content type of the chunk
     * @param text The text
     * @return This EncoderOutput object for call chaining
     */
    public EncoderOutput addChunk(Origin origin, String contentType, CharSequence text) {
        _outputChunks.add(new TextChunk(origin, contentType, String.valueOf(text)));
        return this;
    }

    /**
     * Add a new text chunk to the output of origin {@link Origin#INPUT} and content type "text/plain"
     * @param origin The origin of the chunk
     * @param contentType The content type of the chunk
     * @param text The text
     * @return This EncoderOutput object for call chaining
     */
    public EncoderOutput addChunk(CharSequence text) {
        addChunk(Origin.INPUT, "text/plain", text);
        return this;
        
    }
    
    /**
     * Notifies that the encoder ignores the current text chunk, which will be kept unmodified. No further output should be placed after calling this method.
     * @return This EncoderOutput object for call chaining
     */
    public EncoderOutput ignoreChunk() {
        _ignored = true;
        _outputChunks = Collections.singletonList(_inputChunk);
        return this;
    }
    
    /**
     * Replaces text of the input chunk with the given chunk text, keeping the origin of the original chunk
     * @return This EncoderOutput object for call chaining
     */
    public EncoderOutput replaceChunk(String text) {
       clearChunks();
       addChunk(_inputChunk.getOrigin(), _inputChunk.getContentType(), text);
       return this;
    }

    /**
     * Removes all already added chunks
     * @return This EncoderOutput object for call chaining
     */
    public EncoderOutput clearChunks() {
        _outputChunks.clear();
        return this;
    }

    /**
     * Returns the added chunks
     */
    public List<TextChunk> getOutputChunks() {
        return _outputChunks;
    }

    /**
     * Returns if {@link #ignoreChunk()} was called
     */
    protected boolean isIgnored() {
        return _ignored;
    }
    
    /**
     * Chains another encoder that should run after this encoder run finishes. The encoder will be given the output chunks of this encoder (or of a previously chained controller) and the same WGA object and flags for context.
     * Unlimited encoders can be chained. A frequent use case is to divide up one encoder into multiple sub encoders, because it needs multiple passes on the input to work.  
     * @param encoder The encoder to chain
     * @return This EncoderOutput object for call chaining
     */
    public EncoderOutput chainEncoder(WGAEncoder encoder) {
        _chainedEncoders.add(encoder);
        return this;
    }
    
    /**
     * Replaces parts of the input chunk using a regular expression. The output are multiple chunks where the replaced parts have origin {@link Origin#ADDED} and the unmodified parts have origin {@link Origin#INPUT}
     * @param pattern The regex pattern for replacement
     * @param replacement The replacement string. This supports the group variables known from {@link Matcher#appendReplacement(StringBuffer, String)} on the state of Java SE 6, so the string "$0" will be replaced by the whole matched string, "$1" by the first matching group, "$2" the second and so on.
     * @return This EncoderOutput object for call chaining
     */
    public EncoderOutput replaceChunkViaRegex(Pattern pattern, String replacement) {
     
        Matcher m = pattern.matcher(_inputChunk.getText());
        int lastAppendPosition = 0;
        boolean result = m.find();
        
        if (result) {
            do {
                lastAppendPosition = appendReplacement(_inputChunk.getText(), m, replacement, lastAppendPosition);
                result = m.find();
            } while (result);
            
            
        }
        
        if (_inputChunk.getText().length() >= lastAppendPosition) {
            addChunk(_inputChunk.getText().substring(lastAppendPosition));
        }
        
        return this;
        
    }
    
    private int appendReplacement(String text, Matcher m, String replacement, int lastAppendPosition) {

        MatchResult matchResult = m.toMatchResult();

        int cursor = 0;
        StringBuilder result = new StringBuilder();

        while (cursor < replacement.length()) {
            char nextChar = replacement.charAt(cursor);
            if (nextChar == '\\') {
                cursor++;
                nextChar = replacement.charAt(cursor);
                result.append(nextChar);
                cursor++;
            } else if (nextChar == '$') {
                // Skip past $
                cursor++;
                // The first number is always a group
                int refNum = (int)replacement.charAt(cursor) - '0';
                if ((refNum < 0)||(refNum > 9))
                    throw new IllegalArgumentException(
                        "Illegal group reference");
                cursor++;

                // Capture the largest legal group string
                boolean done = false;
                while (!done) {
                    if (cursor >= replacement.length()) {
                        break;
                    }
                    int nextDigit = replacement.charAt(cursor) - '0';
                    if ((nextDigit < 0)||(nextDigit > 9)) { // not a number
                        break;
                    }
                    int newRefNum = (refNum * 10) + nextDigit;
                    if (m.groupCount() < newRefNum) {
                        done = true;
                    } else {
                        refNum = newRefNum;
                        cursor++;
                    }
                }
                // Append group
                if (matchResult.start(refNum) != -1 && matchResult.end(refNum) != -1)
                    result.append(text, m.start(refNum), m.end(refNum));
            } else {
                result.append(nextChar);
                cursor++;
            }
        }
        // Append the intervening text
        if (m.start() > lastAppendPosition) {
            addChunk(text.subSequence(lastAppendPosition, m.start()));
        }

        // Append the match substitution
        addChunk(Origin.ADDED, "text/html", result.toString());
        return m.end();
    }

    

}
