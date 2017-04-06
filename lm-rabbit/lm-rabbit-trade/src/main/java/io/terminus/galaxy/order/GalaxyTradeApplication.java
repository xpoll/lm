package io.terminus.galaxy.order;

import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Desc:
 * Mail: F@terminus.io
 * Data: 16/3/7
 * Author: yangzefeng
 */
@SpringBootApplication
public class GalaxyTradeApplication {

    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyTradeApplication.class,
                "classpath:/spring/galaxy-trade-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
