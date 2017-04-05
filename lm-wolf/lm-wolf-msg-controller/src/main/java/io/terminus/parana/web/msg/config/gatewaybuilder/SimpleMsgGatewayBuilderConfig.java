package io.terminus.parana.web.msg.config.gatewaybuilder;

import io.terminus.parana.web.msg.MsgGatewayBuilder;
import io.terminus.parana.web.msg.impl.common.simple.SimpleMsgGatewayBuilder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SimpleMsgGatewayBuilderConfig {
   @Bean
   @ConditionalOnMissingBean({MsgGatewayBuilder.class})
   public MsgGatewayBuilder simpleMsgGatewayBuilder() {
      return new SimpleMsgGatewayBuilder();
   }
}
