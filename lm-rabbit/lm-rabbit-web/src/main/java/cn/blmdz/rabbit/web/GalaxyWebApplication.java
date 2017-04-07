/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.web;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-02-01
 */
@SpringBootApplication
public class GalaxyWebApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyWebApplication.class,
                "classpath:/spring/galaxy-web-dubbo-consumer.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
