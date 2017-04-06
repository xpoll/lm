package io.terminus.galaxy.msg;

import io.terminus.parana.msg.impl.MessageAutoConfig;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Created by zhanghecheng on 16/3/8.
 */
@Configuration
@Import({MessageAutoConfig.class})
public class GalaxyMsgConfig {

}
