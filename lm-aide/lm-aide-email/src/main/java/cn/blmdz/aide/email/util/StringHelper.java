package cn.blmdz.aide.email.util;

public class StringHelper {
   public static String singleString2JsonFormat(String from) {
      if(!from.contains("[")) {
         from = "[\"" + from + "\"]";
      }

      return from;
   }
}
