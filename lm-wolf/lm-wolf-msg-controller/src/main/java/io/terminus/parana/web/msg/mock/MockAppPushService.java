package io.terminus.parana.web.msg.mock;

import io.terminus.lib.apppush.AppPushException;
import io.terminus.lib.apppush.AppPushService;
import io.terminus.parana.common.util.MapUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MockAppPushService implements AppPushService {
   private static final Logger log = LoggerFactory.getLogger(MockAppPushService.class);

   public String send(String deviceType, String deviceTokens, String title, String content) throws AppPushException {
      log.info(MapUtil.from().of("deviceType", deviceType, "deviceTokens", deviceTokens, "title", title, "content", content).toString());
      return "true";
   }
}
