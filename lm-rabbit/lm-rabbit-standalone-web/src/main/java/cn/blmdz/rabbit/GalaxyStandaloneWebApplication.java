package cn.blmdz.rabbit;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

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
