package io.terminus.galaxy.pay;

import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @author Effet
 */
@SpringBootApplication
public class GalaxyPayApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyPayApplication.class,
                "classpath:/spring/galaxy-pay-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
