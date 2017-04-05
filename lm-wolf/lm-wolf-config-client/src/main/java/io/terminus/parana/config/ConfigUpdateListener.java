package io.terminus.parana.config;

import com.google.common.base.Throwables;
import com.google.common.eventbus.EventBus;
import io.terminus.parana.config.ConfigCenter;
import io.terminus.parana.config.event.ConfigEvent;
import io.terminus.parana.config.event.Operator;
import io.terminus.zookeeper.ZKClientFactory;
import io.terminus.zookeeper.pubsub.SubscribeCallback;
import io.terminus.zookeeper.pubsub.Subscriber;
import java.util.List;
import javax.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.SerializationUtils;

public class ConfigUpdateListener {
   private static final Logger log = LoggerFactory.getLogger(ConfigUpdateListener.class);
   @Autowired(
      required = false
   )
   private EventBus eventBus;
   @Autowired(
      required = false
   )
   private ZKClientFactory zkClientFactory;
   @Autowired
   private ConfigCenter configCenter;

   @PostConstruct
   public void listen() throws Exception {
      if(this.zkClientFactory != null && this.eventBus != null) {
         Subscriber subscriber = new Subscriber(this.zkClientFactory, "/parana", "config");

         try {
            subscriber.subscribe(new SubscribeCallback() {
               public void fire(byte[] data) {
                  Object obj = SerializationUtils.deserialize(data);
                  if(obj instanceof ConfigEvent) {
                     ConfigUpdateListener.log.info("********* catch curator event {} *****************", obj);
                     ConfigEvent event = (ConfigEvent)obj;
                     if(event.getOp().isPresent() && event.getData().isPresent()) {
                        ConfigUpdateListener.this.configCenter.commit(Operator.from((Integer)event.getOp().get()), (List)event.getData().get());
                     }

                     ConfigUpdateListener.this.eventBus.post(event);
                  }

               }
            });
         } catch (Exception var3) {
            log.warn("failed to subscribe cache event, cause: {}", Throwables.getStackTraceAsString(var3));
         }

      }
   }
}
