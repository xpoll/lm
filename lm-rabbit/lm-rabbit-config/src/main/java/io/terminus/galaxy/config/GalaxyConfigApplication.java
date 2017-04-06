package io.terminus.galaxy.config;


import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;


/**
 * Mail: xiao@terminus.io <br>
 * Date: 2016-03-11 12:52 PM  <br>
 * Author: xiao
 */

@SpringBootApplication
public class GalaxyConfigApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyConfigApplication.class,
                "classpath:/spring/galaxy-config-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}