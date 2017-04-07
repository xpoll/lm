package cn.blmdz.wolf.order.util;

import java.io.Serializable;
import java.util.Map;

public final class Params {
   public static <T extends Serializable> T get(Map<String, T> source, String key) {
      return source.get(key);
   }
}
