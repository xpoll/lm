package cn.blmdz.rabbit.config;


import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;


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