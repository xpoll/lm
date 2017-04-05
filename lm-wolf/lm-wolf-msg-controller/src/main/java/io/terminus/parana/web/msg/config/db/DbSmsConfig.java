package io.terminus.parana.web.msg.config.db;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.web.msg.impl.common.db", "io.terminus.parana.web.msg.impl.sms.common", "io.terminus.parana.web.msg.impl.sms.db"})
public class DbSmsConfig {
}
