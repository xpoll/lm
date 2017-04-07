package cn.blmdz.rabbit.pay.mock;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

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
