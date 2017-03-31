package cn.blmdz.home.session.util;

public class StringUtil {
   public static boolean isEmpty(String str) {
      return str == null || str.length() == 0;
   }

   public static boolean startsWithIgnoreCase(String str, String prefix) {
      if(str != null && prefix != null) {
         if(str.startsWith(prefix)) {
            return true;
         } else if(str.length() < prefix.length()) {
            return false;
         } else {
            String lcStr = str.substring(0, prefix.length()).toLowerCase();
            String lcPrefix = prefix.toLowerCase();
            return lcStr.equals(lcPrefix);
         }
      } else {
         return false;
      }
   }
}
