package io.terminus.parana.auth.core;

import io.terminus.pampas.engine.Setting;
import io.terminus.parana.auth.core.AclLoader;
import io.terminus.parana.auth.core.AuthenticationProperties;
import io.terminus.parana.auth.core.Authenticator;
import io.terminus.parana.auth.core.DefaultAuthenticator;
import io.terminus.parana.auth.core.PermissionHelper;
import io.terminus.parana.auth.model.ParanaThreadVars;
import io.terminus.parana.auth.role.CustomRoleLoaderConfigurer;
import io.terminus.parana.auth.role.CustomRoleLoaderRegistry;
import io.terminus.parana.auth.role.DefaultCustomRoleLoaderConfigurer;
import io.terminus.parana.auth.util.ParanaFileLoaderHelper;
import io.terminus.parana.user.auth.UserRoleLoader;
import java.lang.reflect.InvocationTargetException;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

@Configuration
@ComponentScan({"io.terminus.parana.auth.core", "io.terminus.parana.auth.util"})
@ConditionalOnWebApplication
@EnableAspectJAutoProxy
@EnableConfigurationProperties({AuthenticationProperties.class})
public class AuthenticationConfiguration {
   @Bean
   @ConditionalOnClass({Setting.class})
   public AclLoader aclLoader(AuthenticationProperties authenticationProperties, Setting setting, ParanaFileLoaderHelper paranaFileLoaderHelper) throws ClassNotFoundException, InvocationTargetException, IllegalAccessException {
      authenticationProperties.setDevMode(setting.isDevMode());
      return new AclLoader(authenticationProperties, paranaFileLoaderHelper);
   }

   @Bean
   @ConditionalOnMissingClass({"io.terminus.pampas.engine.Setting"})
   public AclLoader aclLoader(AuthenticationProperties authenticationProperties, ParanaFileLoaderHelper paranaFileLoaderHelper) throws ClassNotFoundException {
      ParanaThreadVars.initNoPampasApp(authenticationProperties.getApp());
      return new AclLoader(authenticationProperties, paranaFileLoaderHelper);
   }

   @Bean
   @ConditionalOnBean({UserRoleLoader.class, CustomRoleLoaderRegistry.class})
   public PermissionHelper permissionHelper(UserRoleLoader userRoleLoader, CustomRoleLoaderRegistry customRoleLoaderRegistry) {
      return new PermissionHelper(userRoleLoader, customRoleLoaderRegistry);
   }

   @ConditionalOnBean({AclLoader.class, PermissionHelper.class})
   @ConditionalOnMissingBean({Authenticator.class})
   @Bean
   public Authenticator authenticator(AclLoader aclLoader, PermissionHelper permissionHelper, AuthenticationProperties authenticationProperties) {
      return new DefaultAuthenticator(aclLoader, permissionHelper, authenticationProperties.getLevel());
   }

   @Configuration
   public static class CustomRoleConfiguration {
      @Bean
      public CustomRoleLoaderRegistry customRoleLoaderRegistry() {
         return new CustomRoleLoaderRegistry();
      }

      @ConditionalOnMissingBean({CustomRoleLoaderConfigurer.class})
      @Bean
      public CustomRoleLoaderConfigurer customRoleLoaderConfigurer(CustomRoleLoaderRegistry customRoleLoaderRegistry) {
         CustomRoleLoaderConfigurer configurer = new DefaultCustomRoleLoaderConfigurer();
         configurer.configureCustomRoleLoader(customRoleLoaderRegistry);
         return configurer;
      }
   }
}
