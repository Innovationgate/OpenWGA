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

/**
 * A factory for creating mime type specific image scalers
 */
public interface MimeTypeSpecificImageScalerFactory {
    
    /**
     * Return a scaler for the given mime type. Should return null if the factory is not able to provide a scaler for that type.
     * @param mimeType The mime type
     * @return The scaler or null if not available for that mime type
     */
    public MimeTypeSpecificImageScaler getForMimeType(String mimeType);

}
