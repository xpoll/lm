package io.terminus.parana.auth.model;

import com.google.common.base.MoreObjects;
import com.google.common.collect.Maps;
import io.terminus.parana.auth.model.App;
import java.util.Locale;
import java.util.Map;

public enum ParanaThreadVars {
   APP,
   DOMAIN,
   HOST,
   LOCALE,
   SEO;

   private static App app;
   private static Map noPampasMap = Maps.newHashMap();
   private static ThreadLocal currentEnv = new ThreadLocal() {
      protected Map initialValue() {
         return Maps.newHashMap();
      }
   };

   public static void initNoPampasApp(App initAPP) {
      app = initAPP;
      noPampasMap.put(APP, initAPP);
   }

   private static Object get(ParanaThreadVars var) {
      return ((Map)currentEnv.get()).get(var) == null?noPampasMap.get(var):((Map)currentEnv.get()).get(var);
   }

   private static void set(ParanaThreadVars var, Object value) {
      ((Map)currentEnv.get()).put(var, value);
   }

   private static void clear(ParanaThreadVars var) {
      ((Map)currentEnv.get()).remove(var);
   }

   public static void clearAll() {
      ((Map)currentEnv.get()).clear();
   }

   public static void setApp(App app) {
      set(APP, app);
   }

   public static App getApp() {
      return (App)get(APP);
   }

   public static void clearApp() {
      clear(APP);
   }

   public static String getAppKey() {
      return getApp().getKey();
   }

   public static void setDomain(String domain) {
      set(DOMAIN, domain);
   }

   public static String getDomain() {
      return (String)MoreObjects.firstNonNull(get(DOMAIN), getApp().getDomain());
   }

   public static void clearDomain() {
      clear(DOMAIN);
   }

   public static void setHost(String host) {
      set(HOST, host);
   }

   public static String getHost() {
      return (String)get(HOST);
   }

   public static void clearHost() {
      clear(HOST);
   }

   public static void setLocale(Locale locale) {
      set(LOCALE, locale);
   }

   public static Locale getLocale() {
      return (Locale)MoreObjects.firstNonNull(get(LOCALE), Locale.getDefault());
   }

   public static void clearLocale() {
      clear(LOCALE);
   }
}
