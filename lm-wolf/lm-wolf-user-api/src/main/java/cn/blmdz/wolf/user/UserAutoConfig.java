package cn.blmdz.wolf.user;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.user.auth.DefaultUserRoleLoader;
import cn.blmdz.wolf.user.auth.UserRoleLoader;
import cn.blmdz.wolf.user.service.UserReadService;

@Configuration
@ComponentScan({"io.terminus.parana.user"})
public class UserAutoConfig {
   @ConditionalOnMissingBean({UserRoleLoader.class})
   @Bean
   public UserRoleLoader userRoleLoader(UserReadService userReadService) {
      return new DefaultUserRoleLoader(userReadService);
   }
}
