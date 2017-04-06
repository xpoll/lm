package io.terminus.galaxy.msg;

import io.terminus.parana.common.banner.ParanaBanner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Created by zhanghecheng on 16/3/8.
 */
@SpringBootApplication
public class GalaxyMsgApplication {
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(GalaxyMsgApplication.class,
                "classpath:/spring/galaxy-msg-dubbo-provider.xml");
        application.setBanner(new ParanaBanner());
        application.run(args);
    }
}
