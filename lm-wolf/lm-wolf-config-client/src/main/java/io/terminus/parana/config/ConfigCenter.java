package io.terminus.parana.config;

import com.google.common.base.Joiner;
import com.google.common.base.Optional;
import com.google.common.base.Splitter;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.model.Response;
import io.terminus.parana.config.event.Operator;
import io.terminus.parana.config.model.Config;
import io.terminus.parana.config.service.ConfigReadService;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class ConfigCenter {
   private static final Logger log = LoggerFactory.getLogger(ConfigCenter.class);
   @Autowired(
      required = false
   )
   private ConfigReadService configReadService;
   private LoadingCache properties = CacheBuilder.newBuilder().expireAfterWrite(3000L, TimeUnit.SECONDS).build(new CacheLoader() {
      public Optional load(String configKey) throws Exception {
         List<String> configs = Splitter.on(":").splitToList(configKey);
         String key = (String)configs.get(0);
         int bizType = Integer.parseInt((String)configs.get(1));
         if(ConfigCenter.this.configReadService == null) {
            String value = System.getProperty(key);
            if(value == null) {
               return Optional.absent();
            } else {
               Config config = new Config();
               config.setBizType(Integer.valueOf(0));
               config.setKey(key);
               config.setValue(value);
               config.setGroup("default");
               config.setDataType("string");
               return Optional.of(config);
            }
         } else {
            Response<Config> res = ConfigCenter.this.configReadService.getUniqueConfig(bizType, key);
            if(!res.isSuccess()) {
               ConfigCenter.log.warn("fail to get config key:{}, bizType:{}, cause:{}", new Object[]{key, Integer.valueOf(bizType), res.getError()});
               return Optional.absent();
            } else {
               return Optional.of(res.getResult());
            }
         }
      }
   });

   private String getConfigKey(String key, int bizType) {
      return Joiner.on(":").join(key, Integer.valueOf(bizType), new Object[0]);
   }

   public Optional get(String key) {
      return this.get(key, 0);
   }

   public Optional get(String key, int bizType) {
      String configKey = this.getConfigKey(key, bizType);

      try {
         Optional<Config> res = (Optional)this.properties.getUnchecked(configKey);
         if(res != null && res.isPresent()) {
            Config config = (Config)res.get();
            return Optional.of(config.getValue());
         } else {
            return Optional.absent();
         }
      } catch (Exception var6) {
         log.error("fail to get key:{} cause:\n {}", key, Throwables.getStackTraceAsString(var6));
         return Optional.absent();
      }
   }

   public Map commit(Operator op, List configs) {
      for(Config config : configs) {
         try {
            this.doCommit(op, config);
         } catch (ExecutionException var6) {
            log.warn("fail to process op:{}, config:{}", op, config);
         }
      }

      return ImmutableMap.copyOf(this.properties.asMap());
   }

   public void doCommit(Operator op, Config cfg) throws ExecutionException {
      if(cfg.getKey() == null || cfg.getBizType() == null) {
         log.warn("config status illegal, so skipped it, {}", cfg);
      }

      String cfgKey = cfg.getKey() + ":" + cfg.getBizType();
      Optional<Config> older = (Optional)this.properties.getIfPresent(cfgKey);
      if(older != null && older.isPresent()) {
         if(op == Operator.UPDATE) {
            log.info("Config {} updated from {} to {}", new Object[]{cfg.getKey(), ((Config)older.get()).getValue(), cfg.getValue()});
            this.properties.invalidate(cfgKey);
         }

         if(op == Operator.DELETE) {
            log.info("Config {} deleted, key:{}, value:{}", cfg.getKey(), ((Config)older.get()).getValue());
            this.properties.invalidate(cfgKey);
         }

         if(op == Operator.CREATE) {
            log.info("Config {} added , key:{}, value:{}, created at:{}", new Object[]{cfg.getKey(), cfg.getValue(), (new DateTime(cfg.getCreatedAt())).toString("yyyy-MM-dd HH:mm:ss")});
            this.properties.invalidate(cfgKey);
         }

      } else {
         log.debug("config {}, bizType:{} not found, so skipped it");
      }
   }

   public LoadingCache getProperties() {
      return this.properties;
   }
}
