package io.terminus.parana.web.msg.config.test;

import io.terminus.parana.web.msg.config.test.TestAppPushWebServiceConfig;
import io.terminus.parana.web.msg.config.test.TestEmailWebServiceConfig;
import io.terminus.parana.web.msg.config.test.TestNotifyWebServiceConfig;
import io.terminus.parana.web.msg.config.test.TestSmsWebServiceConfig;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

@Configuration
@Import({TestNotifyWebServiceConfig.class, TestAppPushWebServiceConfig.class, TestEmailWebServiceConfig.class, TestSmsWebServiceConfig.class})
public class TestAllChannelWebServiceConfig {
}
