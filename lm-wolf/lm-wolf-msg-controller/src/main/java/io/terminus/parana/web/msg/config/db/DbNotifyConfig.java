package io.terminus.parana.web.msg.config.db;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.web.msg.impl.common.db", "io.terminus.parana.web.msg.impl.notify.common", "io.terminus.parana.web.msg.impl.notify.db", "io.terminus.parana.web.msg.impl.notify.controller"})
public class DbNotifyConfig {
}
