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

import java.util.Map;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;

/**
 * Wrapper for an {@link ObjectFormatter} to let it implement {@link WGAEncoder}
 * This one will ignore all text chunks that are not of type 
 */
public class EncodingFormatterEncoder implements WGAInputOnlyEncoder {

    private Context _cx;
    private Map<String, String> _flags;
    private ObjectFormatter _formatter;
    
    public EncodingFormatterEncoder(ObjectFormatter formatter) {
        _formatter = formatter;
    }

    @Override
    public void encode(WGA wga, TextChunk input, EncoderOutput output, Map<String,String> flags) throws WGException {

        try {
            output.replaceChunk(_formatter.format(input.getText()));
        }
        catch (FormattingException e) {
            throw new WGException("Exception in WebTML encoder", e);
        }
        

    }

}
