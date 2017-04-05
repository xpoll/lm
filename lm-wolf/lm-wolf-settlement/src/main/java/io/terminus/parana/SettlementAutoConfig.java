package io.terminus.parana;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.settlement.impl"})
public class SettlementAutoConfig {
}
