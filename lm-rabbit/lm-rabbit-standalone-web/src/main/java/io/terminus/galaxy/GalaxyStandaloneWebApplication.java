package io.terminus.galaxy;

import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @author Effet
 */
@SpringBootApplication
public class GalaxyStandaloneWebApplication {

    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyStandaloneWebApplication.class);
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
