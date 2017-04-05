package io.terminus.parana.user;

import io.terminus.parana.user.auth.DefaultUserRoleLoader;
import io.terminus.parana.user.auth.UserRoleLoader;
import io.terminus.parana.user.service.UserReadService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.user"})
public class UserAutoConfig {
   @ConditionalOnMissingBean({UserRoleLoader.class})
   @Bean
   public UserRoleLoader userRoleLoader(UserReadService userReadService) {
      return new DefaultUserRoleLoader(userReadService);
   }
}
