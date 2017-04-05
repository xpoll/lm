package io.terminus.parana.web.msg.config.simple;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.web.msg.impl.common.simple", "io.terminus.parana.web.msg.impl.notify.common", "io.terminus.parana.web.msg.impl.notify.controller", "io.terminus.parana.web.msg.impl.notify.simple"})
public class SimpleNotifyConfig {
}
