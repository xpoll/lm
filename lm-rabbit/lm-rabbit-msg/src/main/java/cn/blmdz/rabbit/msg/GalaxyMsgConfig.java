package cn.blmdz.rabbit.msg;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.msg.impl.MessageAutoConfig;

/**
 * Created by zhanghecheng on 16/3/8.
 */
@Configuration
@Import({MessageAutoConfig.class})
public class GalaxyMsgConfig {

}
