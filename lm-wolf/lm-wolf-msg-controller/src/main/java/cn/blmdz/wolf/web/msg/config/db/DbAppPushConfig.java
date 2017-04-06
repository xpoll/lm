package cn.blmdz.wolf.web.msg.config.db;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.web.msg.impl.common.db", "io.terminus.parana.web.msg.impl.apppush.common", "io.terminus.parana.web.msg.impl.apppush.db"})
public class DbAppPushConfig {
}
