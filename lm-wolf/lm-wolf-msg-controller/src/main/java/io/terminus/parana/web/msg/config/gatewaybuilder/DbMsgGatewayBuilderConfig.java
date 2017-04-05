package io.terminus.parana.web.msg.config.gatewaybuilder;

import io.terminus.parana.config.ConfigCenter;
import io.terminus.parana.web.msg.MsgGatewayBuilder;
import io.terminus.parana.web.msg.impl.common.db.DbMsgGatewayBuilder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class DbMsgGatewayBuilderConfig {
   @Bean
   @ConditionalOnMissingBean({MsgGatewayBuilder.class})
   public MsgGatewayBuilder dbMsgGatewayBuilder(ConfigCenter configCenter) {
      return new DbMsgGatewayBuilder(configCenter);
   }
}
