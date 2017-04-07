/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.admin;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

import cn.blmdz.wolf.common.banner.ParanaBanner;

/**
 * Author  : panxin
 * Date    : 6:17 PM 2/29/16
 * Mail    : panxin@terminus.io
 */
@EnableScheduling
@SpringBootApplication
public class GalaxyAdminApplication {

    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyAdminApplication.class,
                "classpath:/spring/galaxy-admin-dubbo-consumer.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
