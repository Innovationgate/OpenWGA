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

package de.innovationgate.wga.common.beans.csconfig.v1;

public class MediaKey {
    
    private String key;
    private String mimeType;
    private boolean binary;
    private boolean httpLogin;
    
    public MediaKey() {
        super();
    }

    public MediaKey(String key, String mimeType, boolean binary, boolean httpLogin) {
        super();
        this.key = key.toLowerCase();
        this.mimeType = mimeType;
        this.binary = binary;
        this.httpLogin = httpLogin;
    }
    
    public boolean isBinary() {
        return binary;
    }
    public void setBinary(boolean binary) {
        this.binary = binary;
    }
    public String getKey() {
        return key;
    }
    public void setKey(String key) {
        this.key = key;
    }
    public String getMimeType() {
        return mimeType;
    }
    public void setMimeType(String mimeType) {
        this.mimeType = mimeType;
    }
    public boolean isHttpLogin() {
        return httpLogin;
    }
    public void setHttpLogin(boolean httpLogin) {
        this.httpLogin = httpLogin;
    }

    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + (binary ? 1231 : 1237);
        result = PRIME * result + (httpLogin ? 1231 : 1237);
        result = PRIME * result + ((key == null) ? 0 : key.hashCode());
        result = PRIME * result + ((mimeType == null) ? 0 : mimeType.hashCode());
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final MediaKey other = (MediaKey) obj;
        if (binary != other.binary)
            return false;
        if (httpLogin != other.httpLogin)
            return false;
        if (key == null) {
            if (other.key != null)
                return false;
        }
        else if (!key.equals(other.key))
            return false;
        if (mimeType == null) {
            if (other.mimeType != null)
                return false;
        }
        else if (!mimeType.equals(other.mimeType))
            return false;
        return true;
    }

}
