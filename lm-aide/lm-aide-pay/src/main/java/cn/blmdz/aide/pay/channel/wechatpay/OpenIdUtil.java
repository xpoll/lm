package io.terminus.lib.pay.channel.wechatpay;

import com.google.common.collect.Maps;
import java.util.Map;

public class OpenIdUtil {
   private static ThreadLocal openIdThreadLocal = new ThreadLocal();
   private static ThreadLocal openIdSThreadLocal = new ThreadLocal();

   public static void putOpenId(Long userId, String openId) {
      Map<Long, String> openIdMap = (Map)openIdSThreadLocal.get();
      openIdMap = (Map)(openIdMap == null?Maps.newHashMap():openIdMap);
      openIdMap.put(userId, openId);
      openIdSThreadLocal.set(openIdMap);
   }

   public static void putOpenId(String openId) {
      openIdThreadLocal.set(openId);
   }

   public static String getOpenId(Long userId) {
      return (String)((Map)openIdSThreadLocal.get()).get(userId);
   }

   public static String getOpenId() {
      return (String)openIdThreadLocal.get();
   }

   public static void clearOpenId(Long userId) {
      Map<Long, String> openIdMap = (Map)openIdSThreadLocal.get();
      if(openIdMap != null && !openIdMap.isEmpty()) {
         openIdMap.remove(userId);
      }

   }

   public static void clearOpenId() {
      openIdSThreadLocal.remove();
   }
}
