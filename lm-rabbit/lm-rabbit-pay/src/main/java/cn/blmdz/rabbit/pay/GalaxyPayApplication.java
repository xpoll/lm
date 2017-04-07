package cn.blmdz.rabbit.pay;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

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
