package de.innovationgate.ant;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.LineNumberReader;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

public class ReadCVSPassTask extends Task {
    
    private String _root;
    private String _file;
    private String _host;
    private String _protocol = "pserver";
    private String _user;
    private String _path;
    private String _property;
    
    /** Array contain char conversion data */
    private static final char[] shifts = {
          0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
         16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
        114, 120,  53,  79,  96, 109,  72, 108,  70,  64,  76,  67, 116,  74,  68,  87,
        111,  52,  75, 119,  49,  34,  82,  81,  95,  65, 112,  86, 118, 110, 122, 105,
         41,  57,  83,  43,  46, 102,  40,  89,  38, 103,  45,  50,  42, 123,  91,  35,
        125,  55,  54,  66, 124, 126,  59,  47,  92,  71, 115,  78,  88, 107, 106,  56,
         36, 121, 117, 104, 101, 100,  69,  73,  99,  63,  94,  93,  39,  37,  61,  48,
         58, 113,  32,  90,  44,  98,  60,  51,  33,  97,  62,  77,  84,  80,  85, 223,
        225, 216, 187, 166, 229, 189, 222, 188, 141, 249, 148, 200, 184, 136, 248, 190,
        199, 170, 181, 204, 138, 232, 218, 183, 255, 234, 220, 247, 213, 203, 226, 193,
        174, 172, 228, 252, 217, 201, 131, 230, 197, 211, 145, 238, 161, 179, 160, 212,
        207, 221, 254, 173, 202, 146, 224, 151, 140, 196, 205, 130, 135, 133, 143, 246,
        192, 159, 244, 239, 185, 168, 215, 144, 139, 165, 180, 157, 147, 186, 214, 176,
        227, 231, 219, 169, 175, 156, 206, 198, 129, 164, 150, 210, 154, 177, 134, 127,
        182, 128, 158, 208, 162, 132, 167, 209, 149, 241, 153, 251, 237, 236, 171, 195,
        243, 233, 253, 240, 194, 250, 191, 155, 142, 137, 245, 235, 163, 242, 178, 152
    };
    

    public String getFile() {
        return _file;
    }


    public void setFile(String file) {
        _file = file;
    }


    @Override
    public void execute() throws BuildException {

        try {
            
            String lineStart;
            if (getRoot() != null) {
                lineStart = getRoot();
            }
            else {
                lineStart = ":" + getProtocol() + ":" + getUser() + "@" + getHost() + ":" + getPath();
            }
            LineNumberReader reader = new LineNumberReader(new FileReader(new File(getFile())));
            String line;
            String password = null;
            while ((line = reader.readLine()) != null) {
                
                if (line.startsWith(lineStart)) {
                    password = unscramble(line);
                    break;
                }
                
            }
            reader.close();
            
            if (password != null) {
                getProject().setProperty(getProperty(), password);
            }
            else {
                throw new BuildException("Could not find password line for: " + lineStart);
            }
        }
        catch (Exception e) {
            throw new BuildException(e);
        }
        
    }


    private String unscramble(String line) {

        int passPos = line.indexOf(" A");
        if (passPos == -1) {
            throw new BuildException("Misformatted line " + line);
        }
        
        String mangled = line.substring(passPos + 2);
        
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < mangled.length(); i++) {
            int origChar = findChar(shifts, mangled.charAt(i));
            if (origChar != -1) {
                buf.append((char) origChar);
            }
            else {
                System.out.println("Cannot find char " + mangled.charAt(i));
            }
        }
        return buf.toString();
        
        
    }


    public String getHost() {
        return _host;
    }


    public void setHost(String host) {
        _host = host;
    }


    public String getProtocol() {
        return _protocol;
    }


    public void setProtocol(String protocol) {
        this._protocol = protocol;
    }


    public String getUser() {
        return _user;
    }


    public void setUser(String user) {
        this._user = user;
    }


    public String getPath() {
        return _path;
    }


    public void setPath(String path) {
        this._path = path;
    }


    public String getProperty() {
        return _property;
    }


    public void setProperty(String property) {
        this._property = property;
    }
    
    private static int findChar(char[] shifts2, char charAt) {
        
        for (int idx=0; idx < shifts2.length; idx++) {
            if (shifts[idx] == charAt) {
                return idx;
            }
        }
        
        return -1;
        
    }


    public String getRoot() {
        return _root;
    }


    public void setRoot(String root) {
        _root = root;
    }

}
