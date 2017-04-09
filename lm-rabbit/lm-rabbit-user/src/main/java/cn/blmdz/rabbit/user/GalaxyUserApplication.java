package cn.blmdz.rabbit.user;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class GalaxyUserApplication {
	public static void main(String[] args) {
		SpringApplication application = new SpringApplication(GalaxyUserApplication.class,
				"classpath:/spring/galaxy-user-dubbo-provider.xml");
		application.run(args);
	}
}
