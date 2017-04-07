package cn.blmdz.rabbit.open;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

@SpringBootApplication
public class GalaxyOPApplication {

    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyOPApplication.class,
                "classpath:/spring/galaxy-open-dubbo-consumer.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
