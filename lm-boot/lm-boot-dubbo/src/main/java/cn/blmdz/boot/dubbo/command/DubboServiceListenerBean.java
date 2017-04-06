package cn.blmdz.boot.dubbo.command;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

public class DubboServiceListenerBean implements ApplicationListener, ApplicationContextAware {
   private static final Logger log = LoggerFactory.getLogger(DubboServiceListenerBean.class);
   protected ApplicationContext ctx;
   public String appName;

   public DubboServiceListenerBean(String appName) {
      this.appName = appName;
   }

   public void onApplicationEvent(ApplicationEvent event) {
      if(event instanceof ContextRefreshedEvent && this.appName != null) {
         log.info("{} boot successfully", this.appName);
      }

   }

   public void setApplicationContext(ApplicationContext ctx) throws BeansException {
      this.ctx = ctx;
   }

   public String getAppName() {
      return this.appName;
   }

   public void setAppName(String appName) {
      this.appName = appName;
   }
}
