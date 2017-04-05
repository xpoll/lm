package io.terminus.parana.auth.web;

import io.terminus.parana.auth.core.AuthenticationConfiguration;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.Import;

@Configuration
@ComponentScan
@Import({AuthenticationConfiguration.class})
@ConditionalOnWebApplication
@EnableAspectJAutoProxy
@AutoConfigureAfter({AuthenticationConfiguration.class})
public class WebAuthenticationConfiguration {
}
