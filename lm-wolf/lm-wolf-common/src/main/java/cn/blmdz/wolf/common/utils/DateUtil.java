package cn.blmdz.wolf.common.utils;

import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.DateTimeFormatterBuilder;
import org.joda.time.format.DateTimeParser;
import org.joda.time.format.DateTimePrinter;

public class DateUtil {
   private static final DateTimeParser[] parsers = new DateTimeParser[]{DateTimeFormat.forPattern("yyyy-MM-dd").getParser(), DateTimeFormat.forPattern("yyyyMMdd").getParser()};
   private static final DateTimeFormatter dateFormatter = (new DateTimeFormatterBuilder()).append((DateTimePrinter)null, parsers).toFormatter();

   public static boolean isValidDate(String value) {
      try {
         dateFormatter.parseDateTime(value);
         return true;
      } catch (Exception var2) {
         return false;
      }
   }
}
