package cn.blmdz.wolf.web.msg.config.db;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"cn.blmdz.wolf.web.msg.impl.common.db", "cn.blmdz.wolf.web.msg.impl.notify.common", "cn.blmdz.wolf.web.msg.impl.notify.db", "cn.blmdz.wolf.web.msg.impl.notify.controller"})
public class DbNotifyConfig {
}
