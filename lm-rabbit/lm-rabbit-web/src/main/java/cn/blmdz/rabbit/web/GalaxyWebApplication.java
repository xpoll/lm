package cn.blmdz.rabbit.web;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;

@EnableAutoConfiguration(exclude = DataSourceAutoConfiguration.class)
@SpringBootApplication
public class GalaxyWebApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyWebApplication.class,
                "classpath:/spring/galaxy-web-dubbo-consumer.xml");
        application.run(args);
    }
}
