package io.terminus.parana.config.impl.manager;

import io.terminus.parana.config.impl.dao.ConfigDao;
import io.terminus.parana.config.model.Config;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class ConfigManager {
   private static final Logger log = LoggerFactory.getLogger(ConfigManager.class);
   @Autowired
   private ConfigDao configDao;

   @Transactional
   public boolean bulkCreate(List configs) {
      for(Config config : configs) {
         this.configDao.create(config);
      }

      return Boolean.TRUE.booleanValue();
   }

   @Transactional
   public boolean bulkUpdate(List configs) {
      for(Config config : configs) {
         this.configDao.update(config);
      }

      return Boolean.TRUE.booleanValue();
   }
}
