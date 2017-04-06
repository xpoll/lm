package cn.blmdz.wolf.web.msg.mock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.aide.sms.SmsException;
import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.wolf.common.util.MapUtil;

public class MockSmsService implements SmsService {
   private static final Logger log = LoggerFactory.getLogger(MockSmsService.class);

   public String send(String from, String toes, String message, String extra) throws SmsException {
      log.info(MapUtil.from().of("from", from, "toes", toes, "message", message, "extra", extra).toString());
      return "true";
   }

   public String send(String from, String toes, String message) throws SmsException {
      log.info(MapUtil.from().of("from", from, "toes", toes, "message", message).toString());
      return "true";
   }

   public Integer available() {
      return Integer.valueOf(100000);
   }
}
