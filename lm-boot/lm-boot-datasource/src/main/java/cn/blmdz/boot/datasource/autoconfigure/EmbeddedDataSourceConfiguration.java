package cn.blmdz.boot.datasource.autoconfigure;

import javax.annotation.PreDestroy;

import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;

import cn.blmdz.boot.datasource.autoconfigure.EmbeddedDatabaseConnection;
import cn.blmdz.boot.datasource.properties.DataSourceProperties;

@Configuration
@EnableConfigurationProperties({DataSourceProperties.class})
public class EmbeddedDataSourceConfiguration implements BeanClassLoaderAware {
   private EmbeddedDatabase database;
   private ClassLoader classLoader;
   @Autowired
   private DataSourceProperties properties;

   public void setBeanClassLoader(ClassLoader classLoader) {
      this.classLoader = classLoader;
   }

   @Bean
   public EmbeddedDatabase dataSource() {
      EmbeddedDatabaseBuilder builder = (new EmbeddedDatabaseBuilder()).setType(EmbeddedDatabaseConnection.get(this.classLoader).getType());
      this.database = builder.setName(this.properties.getName()).build();
      return this.database;
   }

   @PreDestroy
   public void close() {
      if(this.database != null) {
         this.database.shutdown();
      }

   }
}
