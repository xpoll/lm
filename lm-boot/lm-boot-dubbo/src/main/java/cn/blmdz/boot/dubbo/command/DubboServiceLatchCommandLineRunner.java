package cn.blmdz.boot.dubbo.command;

import java.util.concurrent.CountDownLatch;
import org.springframework.boot.CommandLineRunner;

public class DubboServiceLatchCommandLineRunner implements CommandLineRunner {
	public void run(String... args) throws Exception {
		CountDownLatch countDownLatch = new CountDownLatch(1);
		countDownLatch.await();
	}
}
