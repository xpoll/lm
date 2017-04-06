package cn.blmdz.wolf.web.msg.config.test;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.web.msg.config.test.TestAppPushWebServiceConfig;
import cn.blmdz.wolf.web.msg.config.test.TestEmailWebServiceConfig;
import cn.blmdz.wolf.web.msg.config.test.TestNotifyWebServiceConfig;
import cn.blmdz.wolf.web.msg.config.test.TestSmsWebServiceConfig;

@Configuration
@Import({TestNotifyWebServiceConfig.class, TestAppPushWebServiceConfig.class, TestEmailWebServiceConfig.class, TestSmsWebServiceConfig.class})
public class TestAllChannelWebServiceConfig {
}
