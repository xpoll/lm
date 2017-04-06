package cn.blmdz.boot.datasource.autoconfigure;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.datasource.init.DatabasePopulatorUtils;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.util.StringUtils;

import cn.blmdz.boot.datasource.autoconfigure.DataSourceInitializedEvent;
import cn.blmdz.boot.datasource.properties.DataSourceProperties;

public class DataSourceInitializer implements ApplicationListener<DataSourceInitializedEvent> {
   private static Log logger = LogFactory.getLog(DataSourceInitializer.class);
   @Autowired
   private ConfigurableApplicationContext applicationContext;
   private DataSource dataSource;
   @Autowired
   private DataSourceProperties properties;
   private boolean initialized = false;

   @PostConstruct
   public void init() {
      if(!this.properties.isInitialize()) {
         logger.debug("Initialization disabled (not running DDL scripts)");
      } else {
         if(this.applicationContext.getBeanNamesForType(DataSource.class, false, false).length > 0) {
            this.dataSource = (DataSource)this.applicationContext.getBean(DataSource.class);
         }

         if(this.dataSource == null) {
            logger.debug("No DataSource found so not initializing");
         } else {
            this.runSchemaScripts();
         }
      }
   }

   private void runSchemaScripts() {
      List<Resource> scripts = this.getScripts(this.properties.getSchema(), "schema");
      if(!scripts.isEmpty()) {
         this.runScripts(scripts);

         try {
            this.applicationContext.publishEvent(new DataSourceInitializedEvent(this.dataSource));
            if(!this.initialized) {
               this.runDataScripts();
               this.initialized = true;
            }
         } catch (IllegalStateException var3) {
            logger.warn("Could not send event to complete DataSource initialization (" + var3.getMessage() + ")");
         }
      }

   }

   public void onApplicationEvent(DataSourceInitializedEvent event) {
      if(!this.properties.isInitialize()) {
         logger.debug("Initialization disabled (not running data scripts)");
      } else {
         if(!this.initialized) {
            this.runDataScripts();
            this.initialized = true;
         }

      }
   }

   private void runDataScripts() {
      List<Resource> scripts = this.getScripts(this.properties.getData(), "data");
      this.runScripts(scripts);
   }

   private List getScripts(String locations, String fallback) {
      if(locations == null) {
         String platform = this.properties.getPlatform();
         locations = "classpath*:" + fallback + "-" + platform + ".sql,";
         locations = locations + "classpath*:" + fallback + ".sql";
      }

      return this.getResources(locations);
   }

   private List getResources(String locations) {
      List<Resource> resources = new ArrayList();

      for(String location : StringUtils.commaDelimitedListToStringArray(locations)) {
         try {
            for(Resource resource : this.applicationContext.getResources(location)) {
               if(resource.exists()) {
                  resources.add(resource);
               }
            }
         } catch (IOException var11) {
            throw new IllegalStateException("Unable to load resource from " + location, var11);
         }
      }

      return resources;
   }

   private void runScripts(List<Resource> resources) {
      if(!resources.isEmpty()) {
         ResourceDatabasePopulator populator = new ResourceDatabasePopulator();
         populator.setContinueOnError(this.properties.isContinueOnError());
         populator.setSeparator(this.properties.getSeparator());
         if(this.properties.getSqlScriptEncoding() != null) {
            populator.setSqlScriptEncoding(this.properties.getSqlScriptEncoding().name());
         }

         for(Resource resource : resources) {
            populator.addScript(resource);
         }

         DatabasePopulatorUtils.execute(populator, this.dataSource);
      }
   }
}
