package io.terminus.galaxy.open;

import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class GalaxyOPApplication {

    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyOPApplication.class,
                "classpath:/spring/galaxy-open-dubbo-consumer.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
