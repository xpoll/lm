package io.terminus.parana.web.msg.config.simple;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.web.msg.impl.common.simple", "io.terminus.parana.web.msg.impl.email.common", "io.terminus.parana.web.msg.impl.email.simple"})
public class SimpleEmailConfig {
}
