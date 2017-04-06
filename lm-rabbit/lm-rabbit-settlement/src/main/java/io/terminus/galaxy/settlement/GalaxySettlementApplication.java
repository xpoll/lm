package io.terminus.galaxy.settlement;

import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @author Effet
 */
@SpringBootApplication
public class GalaxySettlementApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxySettlementApplication.class,
                "classpath:/spring/galaxy-settlement-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
