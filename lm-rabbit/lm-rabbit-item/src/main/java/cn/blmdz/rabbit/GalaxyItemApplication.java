package cn.blmdz.rabbit;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

/**
 * @author Effet
 */
@SpringBootApplication
public class GalaxyItemApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyItemApplication.class,
                "classpath:/spring/galaxy-item-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
