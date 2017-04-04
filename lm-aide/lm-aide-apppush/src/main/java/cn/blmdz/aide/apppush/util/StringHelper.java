package cn.blmdz.aide.apppush.util;

public class StringHelper {
   public static String singleString2JsonFormat(String from) {
      if(!from.contains("[")) {
         from = "[\"" + from + "\"]";
      }

      return from;
   }
}
