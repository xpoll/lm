package io.terminus.parana.web.msg.mock;

import io.terminus.lib.email.EmailException;
import io.terminus.lib.email.EmailService;
import io.terminus.parana.common.util.MapUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MockEmailService implements EmailService {
   private static final Logger log = LoggerFactory.getLogger(MockEmailService.class);

   public String send(String subject, String content, String toes, String attachments) throws EmailException {
      log.info(MapUtil.from().of("subject", subject, "content", content, "toes", toes, "attach", attachments).toString());
      return "true";
   }
}
