package cn.blmdz.wolf.common.util;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

public class MapUtil {
   private Map map;
   private boolean ignoreNullValue = false;

   private MapUtil() {
      this.map = new HashMap();
   }

   private MapUtil(Map map) {
      this.map = map;
   }

   public static MapUtil from() {
      return new MapUtil();
   }

   public static MapUtil from(Map map) {
      return new MapUtil(map);
   }

   public static MapUtil hashMap() {
      return from(new HashMap());
   }

   public static MapUtil treeMap() {
      return from(new TreeMap());
   }

   public MapUtil ignoreNullValue() {
      this.ignoreNullValue = true;
      return this;
   }

   public MapUtil of(Object key, Object value) {
      if(this.ignoreNullValue && value == null) {
         return this;
      } else {
         this.map.put(key, value);
         return this;
      }
   }

   public MapUtil of(Object k1, Object v1, Object k2, Object v2) {
      return this.of(k1, v1).of(k2, v2);
   }

   public MapUtil of(Object k1, Object v1, Object k2, Object v2, Object k3, Object v3) {
      return this.of(k1, v1).of(k2, v2).of(k3, v3);
   }

   public MapUtil of(Object k1, Object v1, Object k2, Object v2, Object k3, Object v3, Object k4, Object v4) {
      return this.of(k1, v1).of(k2, v2).of(k3, v3).of(k4, v4);
   }

   public MapUtil of(Object k1, Object v1, Object k2, Object v2, Object k3, Object v3, Object k4, Object v4, Object k5, Object v5) {
      return this.of(k1, v1).of(k2, v2).of(k3, v3).of(k4, v4).of(k5, v5);
   }

   public Map toMap() {
      return this.map;
   }

   public String toString() {
      return this.map.toString();
   }
}
