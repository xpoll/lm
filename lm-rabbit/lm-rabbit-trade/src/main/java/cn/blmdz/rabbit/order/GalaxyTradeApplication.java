package cn.blmdz.rabbit.order;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

@SpringBootApplication
public class GalaxyTradeApplication {

    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyTradeApplication.class,
                "classpath:/spring/galaxy-trade-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
