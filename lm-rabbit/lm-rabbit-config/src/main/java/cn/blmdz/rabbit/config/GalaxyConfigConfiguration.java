package cn.blmdz.rabbit.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.config.impl.ConfigAutoConfig;

/**
 * Mail: xiao@terminus.io <br>
 * Date: 2016-03-11 12:52 PM  <br>
 * Author: xiao
 */
@Configuration
@Import({ConfigAutoConfig.class})
public class GalaxyConfigConfiguration {

}
