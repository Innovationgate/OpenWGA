package de.innovationgate.testclasses2;

import java.text.ParseException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;

public class NativeObjectMapTest {
    
    public static void doPut(Map<String,Object> map) {
        map.put("that", "is the value");
    }
    
    public static void doPutAll(Map<String,Object> map) throws IllegalStateException, WGException, ParseException {
        
        Map<String,Object> otherMap = new HashMap<String, Object>();
        otherMap.put("a" , 1);
        otherMap.put("b", "String");
        otherMap.put("c", Boolean.TRUE);
        otherMap.put("d", WGA.get().parseDate("01.01.2012", "dd.MM.yyyy"));
        map.putAll(otherMap);
        
    }
    
    public static void doRemove(Map<String,Object> map) {
        map.remove("c");
    }
    
    public static void doClear(Map<String,Object> map) {
        map.clear();
    }
    
    public static int mapSize(Map<String,Object> map) {
        return map.size();
    }

}
