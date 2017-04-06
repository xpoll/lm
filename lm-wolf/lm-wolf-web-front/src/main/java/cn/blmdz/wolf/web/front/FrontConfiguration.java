package cn.blmdz.wolf.web.front;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import cn.blmdz.wolf.web.core.CoreWebConfiguration;

@Configuration
@ComponentScan
@EnableWebMvc
@EnableAutoConfiguration
@Import({CoreWebConfiguration.class})
public class FrontConfiguration {
}
