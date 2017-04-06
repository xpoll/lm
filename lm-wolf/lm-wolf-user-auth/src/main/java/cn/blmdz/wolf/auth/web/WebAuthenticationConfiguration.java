package cn.blmdz.wolf.auth.web;

import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.auth.core.AuthenticationConfiguration;

@Configuration
@ComponentScan
@Import({AuthenticationConfiguration.class})
@ConditionalOnWebApplication
@EnableAspectJAutoProxy
@AutoConfigureAfter({AuthenticationConfiguration.class})
public class WebAuthenticationConfiguration {
}
