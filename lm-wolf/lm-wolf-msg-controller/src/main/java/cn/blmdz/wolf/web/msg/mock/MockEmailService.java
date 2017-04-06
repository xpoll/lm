package cn.blmdz.wolf.web.msg.mock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.aide.email.EmailException;
import cn.blmdz.aide.email.EmailService;
import cn.blmdz.wolf.common.util.MapUtil;

public class MockEmailService implements EmailService {
   private static final Logger log = LoggerFactory.getLogger(MockEmailService.class);

   public String send(String subject, String content, String toes, String attachments) throws EmailException {
      log.info(MapUtil.from().of("subject", subject, "content", content, "toes", toes, "attach", attachments).toString());
      return "true";
   }
}
