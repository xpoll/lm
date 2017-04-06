package cn.blmdz.wolf.web.msg.config.gatewaybuilder;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.impl.common.simple.SimpleMsgGatewayBuilder;

@Configuration
public class SimpleMsgGatewayBuilderConfig {
   @Bean
   @ConditionalOnMissingBean({MsgGatewayBuilder.class})
   public MsgGatewayBuilder simpleMsgGatewayBuilder() {
      return new SimpleMsgGatewayBuilder();
   }
}
