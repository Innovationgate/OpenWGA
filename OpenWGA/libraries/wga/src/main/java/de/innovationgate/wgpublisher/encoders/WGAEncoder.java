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

import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;

/**
 * Encoder used to transform a text chunk, used either via attribute "encode" in WebTML or via method WGA.encode() in TMLScript or on the WGA Server API. 
 * Please consider using {@link WGAInputOnlyEncoder} if you only want to transform text chunks of origin INPUT (as is mostly the case).
 * The same encoder instance is used for encoding all incoming text chunks of an input to be encoded, so the instance may keep state between those invocations.   
 */
public interface WGAEncoder {
    
    /**
     * Encodes a text chunk, using the given {@link EncoderOutput} object to place the result. The method should do one of the following things:
     * <ul>
     * <li>Call {@link EncoderOutput#ignoreChunk()} if it is not interested in this chunk. The chunk will be kept unmodified
     * <li>Call {@link EncoderOutput#replaceChunk(String)} if it wants to replace this chunk with another text chunk completely
     * <li>Call {@link EncoderOutput#addChunk(CharSequence)} an arbitrary number of times if it wants to break up the input chunk into multiple output chunks. In many cases this can also be done conveniently using {@link EncoderOutput#replaceChunkViaRegex(java.util.regex.Pattern, String)} using a regular expression. 
     * </ul>
     * If the method exits without placing any output on the {@link EncoderOutput} object then the input chunk will be swallowed and not put out.
     * @param wga WGA context object
     * @param input The input chunk
     * @param output Object to place encoding output
     * @param flags Flags given to the encoder and their values. Flags given without a value have the value "true" here.
     * @throws WGException
     */
    public void encode(WGA wga, TextChunk input, EncoderOutput output, Map<String,String> flags) throws WGException;

}
