package cn.blmdz.wolf.config.impl.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.config.impl.dao.ConfigDao;
import cn.blmdz.wolf.config.model.Config;
import cn.blmdz.wolf.config.service.ConfigReadService;

@Service
public class ConfigReadServiceImpl implements ConfigReadService {
   private static final Logger log = LoggerFactory.getLogger(ConfigReadServiceImpl.class);
   @Autowired
   private ConfigDao configDao;

   public Response list() {
      try {
         return Response.ok(this.configDao.list(new Config()));
      } catch (Exception var2) {
         log.error("fail to query config by key: {}, cause:\n {}", Throwables.getStackTraceAsString(var2));
         return Response.fail("config.query.fail");
      }
   }

   public Response findByKey(String key) {
      try {
         return Response.ok(this.configDao.findByKey(key));
      } catch (Exception var3) {
         log.error("fail to query config by key: {}, cause:\n {}", key, Throwables.getStackTraceAsString(var3));
         return Response.fail("config.query.fail");
      }
   }

   public Response findByBizType(int bizType) {
      try {
         return Response.ok(this.configDao.findByBizType(bizType));
      } catch (Exception var3) {
         log.error("fail to query config by bizType: {}, cause:\n {}", Integer.valueOf(bizType), Throwables.getStackTraceAsString(var3));
         return Response.fail("config.query.fail");
      }
   }

   public Response findByGroup(String group) {
      try {
         return Response.ok(this.configDao.findByGroup(group));
      } catch (Exception var3) {
         log.error("fail to query config by bizType: {}, cause:\n {}", group, Throwables.getStackTraceAsString(var3));
         return Response.fail("config.query.fail");
      }
   }

   public Response getUniqueConfig(int bizType, String key) {
      try {
         Config config = this.configDao.findByUniqueIndex(bizType, key);
         if(config == null) {
            log.warn("config not found, bizType={}, key={}", Integer.valueOf(bizType), key);
            return Response.fail("config.not.found");
         } else {
            return Response.ok(config);
         }
      } catch (Exception var4) {
         log.error("fail to query config by bizType: {}, key: {} cause:\n {}", new Object[]{Integer.valueOf(bizType), key, Throwables.getStackTraceAsString(var4)});
         return Response.fail("config.query.fail");
      }
   }
}
