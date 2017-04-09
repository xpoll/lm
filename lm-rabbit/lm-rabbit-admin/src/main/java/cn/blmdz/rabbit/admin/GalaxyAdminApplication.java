package cn.blmdz.rabbit.admin;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

@EnableScheduling
@SpringBootApplication
public class GalaxyAdminApplication {

    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyAdminApplication.class,
                "classpath:/spring/galaxy-admin-dubbo-consumer.xml");
        application.run(args);
    }
}
