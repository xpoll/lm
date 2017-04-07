package cn.blmdz.rabbit.settlement;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

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
