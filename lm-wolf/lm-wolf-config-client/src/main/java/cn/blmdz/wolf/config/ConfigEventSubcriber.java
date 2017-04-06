package cn.blmdz.wolf.config;

import com.google.common.eventbus.Subscribe;

import cn.blmdz.wolf.config.ConfigHandler;
import cn.blmdz.wolf.config.event.ConfigEvent;

import java.util.Map;
import java.util.Map.Entry;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class ConfigEventSubcriber implements ApplicationContextAware {
   private ApplicationContext ctx;

   @Subscribe
   public void listenTestEvent(ConfigEvent event) {
      Map<String, ConfigHandler> handlers = this.ctx.getBeansOfType(ConfigHandler.class);

      for(Entry<String, ConfigHandler> handler : handlers.entrySet()) {
         try {
            ((ConfigHandler)handler.getValue()).handle(event);
         } catch (Exception var6) {
            ;
         }
      }

   }

   public void setApplicationContext(ApplicationContext ctx) throws BeansException {
      this.ctx = ctx;
   }
}
