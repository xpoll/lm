package cn.blmdz.wolf.web.msg.config.db;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"cn.blmdz.wolf.web.msg.impl.common.db", "cn.blmdz.wolf.web.msg.impl.sms.common", "cn.blmdz.wolf.web.msg.impl.sms.db"})
public class DbSmsConfig {
}
