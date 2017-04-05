package io.terminus.parana.web.admin;

import io.terminus.parana.web.core.CoreWebConfiguration;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@Configuration
@ComponentScan
@EnableWebMvc
@EnableScheduling
@EnableAutoConfiguration
@Import({CoreWebConfiguration.class})
public class AdminConfiguration {
}
