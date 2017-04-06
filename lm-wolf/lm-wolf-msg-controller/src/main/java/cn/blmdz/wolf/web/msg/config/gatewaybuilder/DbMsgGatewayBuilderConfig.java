package cn.blmdz.wolf.web.msg.config.gatewaybuilder;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.config.ConfigCenter;
import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.impl.common.db.DbMsgGatewayBuilder;

@Configuration
public class DbMsgGatewayBuilderConfig {
   @Bean
   @ConditionalOnMissingBean({MsgGatewayBuilder.class})
   public MsgGatewayBuilder dbMsgGatewayBuilder(ConfigCenter configCenter) {
      return new DbMsgGatewayBuilder(configCenter);
   }
}
