package io.terminus.parana.settlement.utils;

import java.util.Date;
import org.joda.time.DateTime;
import org.joda.time.Days;

public class DateUtils {
   public static int daysBetween(Date start, Date end) {
      DateTime dateTimeStart = new DateTime(start);
      DateTime dateTimeEnd = new DateTime(end);
      return Days.daysBetween(dateTimeStart, dateTimeEnd).getDays();
   }
}
