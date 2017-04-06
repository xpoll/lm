package cn.blmdz.wolf.web.msg.mock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.aide.apppush.AppPushException;
import cn.blmdz.aide.apppush.AppPushService;
import cn.blmdz.wolf.common.util.MapUtil;

public class MockAppPushService implements AppPushService {
   private static final Logger log = LoggerFactory.getLogger(MockAppPushService.class);

   public String send(String deviceType, String deviceTokens, String title, String content) throws AppPushException {
      log.info(MapUtil.from().of("deviceType", deviceType, "deviceTokens", deviceTokens, "title", title, "content", content).toString());
      return "true";
   }
}
