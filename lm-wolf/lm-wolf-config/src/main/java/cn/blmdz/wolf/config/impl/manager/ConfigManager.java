package cn.blmdz.wolf.config.impl.manager;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import cn.blmdz.wolf.config.impl.dao.ConfigDao;
import cn.blmdz.wolf.config.model.Config;

@Component
public class ConfigManager {
   private static final Logger log = LoggerFactory.getLogger(ConfigManager.class);
   @Autowired
   private ConfigDao configDao;

   @Transactional
   public boolean bulkCreate(List<Config> configs) {
      for(Config config : configs) {
         this.configDao.create(config);
      }

      return Boolean.TRUE.booleanValue();
   }

   @Transactional
   public boolean bulkUpdate(List<Config> configs) {
      for(Config config : configs) {
         this.configDao.update(config);
      }

      return Boolean.TRUE.booleanValue();
   }
}
