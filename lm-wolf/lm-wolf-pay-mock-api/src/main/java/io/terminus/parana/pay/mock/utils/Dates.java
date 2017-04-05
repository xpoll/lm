package io.terminus.parana.pay.mock.utils;

import java.util.Date;
import org.joda.time.DateTime;

public class Dates {
   public static Date startOfDay(Date date) {
      return date == null?null:(new DateTime(date)).withTimeAtStartOfDay().toDate();
   }

   public static Date endOfDay(Date date) {
      return date == null?null:(new DateTime(date)).withTimeAtStartOfDay().plusDays(1).toDate();
   }
}
