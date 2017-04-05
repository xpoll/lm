package io.terminus.parana.config.impl.service;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.parana.config.event.ConfigEventWrapper;
import io.terminus.parana.config.event.Operator;
import io.terminus.parana.config.impl.dao.ConfigDao;
import io.terminus.parana.config.impl.manager.ConfigManager;
import io.terminus.parana.config.model.Config;
import io.terminus.parana.config.service.ConfigWriteService;
import io.terminus.zookeeper.pubsub.Publisher;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.SerializationUtils;

@Service
public class ConfigWriteServiceImpl implements ConfigWriteService {
   private static final Logger log = LoggerFactory.getLogger(ConfigWriteServiceImpl.class);
   @Autowired
   private ConfigDao configDao;
   @Autowired
   private ConfigManager configManager;
   @Autowired
   private Publisher publisher;

   public Response create(Config config) {
      try {
         this.configDao.create(config);

         try {
            this.publisher.publish(SerializationUtils.serialize(ConfigEventWrapper.wrapperEvent(Operator.CREATE, Lists.newArrayList(new Config[]{config}))));
         } catch (IllegalStateException var3) {
            log.warn("fail to publish config {} change event, cause: {}", config, var3.getMessage());
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("fail to create config:{}, cause: \n {}", config, Throwables.getStackTraceAsString(var4));
         return Response.fail("config.create.fail");
      }
   }

   public Response bulkCreate(List configs) {
      try {
         return Response.ok(Boolean.valueOf(this.configManager.bulkCreate(configs)));
      } catch (Exception var3) {
         log.error("fail to create configs:{}, cause: \n {}", configs, Throwables.getStackTraceAsString(var3));
         return Response.fail("config.bulk.create.fail");
      }
   }

   public Response update(Config config) {
      try {
         Assert.notNull(config.getId());
         this.configDao.update(config);
         Config newer = (Config)this.configDao.findById(config.getId());

         try {
            this.publisher.publish(SerializationUtils.serialize(ConfigEventWrapper.wrapperEvent(Operator.UPDATE, Lists.newArrayList(new Config[]{newer}))));
         } catch (IllegalStateException var4) {
            log.warn("fail to publish config {} change event, cause: {}", config, var4.getMessage());
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("fail to update config:{}, cause: \n {}", config, Throwables.getStackTraceAsString(var5));
         return Response.fail("config.update.fail");
      }
   }

   public Response bulkUpdate(List configs) {
      try {
         return Response.ok(Boolean.valueOf(this.configManager.bulkUpdate(configs)));
      } catch (Exception var3) {
         log.error("fail to update configs:{}, cause: \n {}", configs, Throwables.getStackTraceAsString(var3));
         return Response.fail("config.bulk.update.fail");
      }
   }

   public Response delete(Long id) {
      try {
         Config deleting = (Config)this.configDao.findById(id);
         this.configDao.delete(id);

         try {
            this.publisher.publish(SerializationUtils.serialize(ConfigEventWrapper.wrapperEvent(Operator.DELETE, Lists.newArrayList(new Config[]{deleting}))));
         } catch (IllegalStateException var4) {
            log.warn("fail to publish config {} change event, cause: {}", deleting, var4.getMessage());
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("fail to delete config with id:{}, cause: \n {}", id, Throwables.getStackTraceAsString(var5));
         return Response.fail("config.delete.fail");
      }
   }
}
