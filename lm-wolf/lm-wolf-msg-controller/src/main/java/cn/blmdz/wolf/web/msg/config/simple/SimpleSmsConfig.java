package cn.blmdz.wolf.web.msg.config.simple;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"cn.blmdz.wolf.web.msg.impl.common.simple", "cn.blmdz.wolf.web.msg.impl.sms.common", "cn.blmdz.wolf.web.msg.impl.sms.simple"})
public class SimpleSmsConfig {
}
