package cn.blmdz.hunt.engine.config;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.hunt.engine.Setting;
import cn.blmdz.hunt.engine.config.model.BackConfig;
import cn.blmdz.hunt.engine.config.model.Component;
import cn.blmdz.hunt.engine.config.model.FrontConfig;
import cn.blmdz.hunt.engine.config.model.Render;
import cn.blmdz.hunt.engine.config.model.Service;
import cn.blmdz.hunt.engine.model.App;

@org.springframework.stereotype.Component
public class ConfigManager implements Runnable {
   private static final Logger log = LoggerFactory.getLogger(ConfigManager.class);
   private final ConcurrentMap frontConfigMap = Maps.newConcurrentMap();
   private final ConcurrentMap backConfigMap = Maps.newConcurrentMap();
   private final ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
   @Autowired
   private Setting setting;
   @Autowired
   private ConfigParser configParser;

   @PostConstruct
   private void init() {
      this.load();
      if(this.setting.isDevMode()) {
         this.executorService.scheduleAtFixedRate(this, 5L, 5L, TimeUnit.SECONDS);
      } else {
         this.executorService.scheduleAtFixedRate(this, 5L, 5L, TimeUnit.MINUTES);
      }

   }

   public List<App> listAllApp() {
      return this.setting.getApps();
   }

   public FrontConfig getFrontConfig(String appKey) {
      return (FrontConfig)this.frontConfigMap.get(appKey);
   }

   public BackConfig getBackConfig(String appKey) {
      return (BackConfig)this.backConfigMap.get(appKey);
   }

   public Service findService(String appKey, String serviceKey) {
      BackConfig backConfig = (BackConfig)this.backConfigMap.get(appKey);
      return backConfig != null && backConfig.getServices() != null?(Service)backConfig.getServices().get(serviceKey):null;
   }

   public Set findMappings(String appKey) {
      FrontConfig frontConfig = (FrontConfig)this.frontConfigMap.get(appKey);
      return frontConfig == null?null:frontConfig.getMappings();
   }

   public List findComponents(String appKey) {
      FrontConfig frontConfig = (FrontConfig)this.frontConfigMap.get(appKey);
      if(frontConfig == null) {
         return Lists.newArrayList();
      } else {
         Map<String, Component> components = frontConfig.getComponents();
         return components != null && !components.isEmpty()?Lists.newArrayList(components.values()):Lists.newArrayList();
      }
   }

   public Component findComponent(String appKey, String componentPath) {
      FrontConfig frontConfig = (FrontConfig)this.frontConfigMap.get(appKey);
      return frontConfig != null && frontConfig.getComponents() != null?(Component)frontConfig.getComponents().get(componentPath):null;
   }

   public List findComponentsByCategory(String appKey, String category) {
      FrontConfig frontConfig = (FrontConfig)this.frontConfigMap.get(appKey);
      return (List)(frontConfig != null && frontConfig.getComponentCategoryListMap() != null?(List)frontConfig.getComponentCategoryListMap().get(category):Lists.newArrayList());
   }

   public Render findRender(String appKey) {
      FrontConfig frontConfig = (FrontConfig)this.frontConfigMap.get(appKey);
      return frontConfig == null?null:frontConfig.getRender();
   }

   private synchronized void load() {
      List<App> apps = this.listAllApp();
      int errorCount = 0;

      for(App app : apps) {
         try {
            this.loadByApp(app);
         } catch (Exception var6) {
            log.warn("error when load config for app: {}", app.getKey(), var6);
            ++errorCount;
         }
      }

      if(errorCount == apps.size()) {
         throw new RuntimeException("all apps config load failed");
      }
   }

   private void loadByApp(App app) throws IllegalAccessException, IOException, InstantiationException {
      String appKey = app.getKey();
      FrontConfig frontConfig = (FrontConfig)this.configParser.parseConfig(app, FrontConfig.class);
      if(frontConfig != null) {
         this.frontConfigMap.put(appKey, frontConfig);
      }

      BackConfig backConfig = (BackConfig)this.configParser.parseConfig(app, BackConfig.class);
      if(backConfig != null) {
         this.backConfigMap.put(appKey, backConfig);
      }

   }

   public void run() {
      try {
         this.load();
      } catch (Exception var2) {
         log.error("error happened when config load heartbeat", var2);
      }

   }
}
