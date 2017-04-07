package cn.blmdz.rabbit.msg;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import cn.blmdz.wolf.common.banner.ParanaBanner;

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
