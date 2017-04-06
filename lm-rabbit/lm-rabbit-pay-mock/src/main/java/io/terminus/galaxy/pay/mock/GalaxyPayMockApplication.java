package io.terminus.galaxy.pay.mock;

import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @author Effet
 */
@SpringBootApplication
public class GalaxyPayMockApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyPayMockApplication.class,
                "classpath:/spring/galaxy-pay-mock-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
