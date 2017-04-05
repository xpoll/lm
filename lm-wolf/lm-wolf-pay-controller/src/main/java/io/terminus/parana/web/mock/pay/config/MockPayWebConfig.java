package io.terminus.parana.web.mock.pay.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnProperty(
   name = {"pay.debug"},
   havingValue = "true"
)
@ComponentScan({"io.terminus.parana.web.mock.pay"})
public class MockPayWebConfig {
}
