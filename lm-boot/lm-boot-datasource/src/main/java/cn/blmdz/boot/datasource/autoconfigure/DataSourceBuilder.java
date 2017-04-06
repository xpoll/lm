package cn.blmdz.boot.datasource.autoconfigure;

import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.MutablePropertyValues;
import org.springframework.boot.bind.RelaxedDataBinder;
import org.springframework.util.ClassUtils;

import cn.blmdz.boot.datasource.autoconfigure.DatabaseDriver;

public class DataSourceBuilder {
   private static final String[] DATA_SOURCE_TYPE_NAMES = new String[]{"org.apache.tomcat.jdbc.pool.DataSource", "com.zaxxer.hikari.HikariDataSource", "org.apache.commons.dbcp.BasicDataSource", "org.apache.commons.dbcp2.BasicDataSource"};
   private Class type;
   private ClassLoader classLoader;
   private Map properties = new HashMap();

   public static DataSourceBuilder create() {
      return new DataSourceBuilder((ClassLoader)null);
   }

   public static DataSourceBuilder create(ClassLoader classLoader) {
      return new DataSourceBuilder(classLoader);
   }

   public DataSourceBuilder(ClassLoader classLoader) {
      this.classLoader = classLoader;
   }

   public DataSource build() {
      Class<? extends DataSource> type = this.getType();
      DataSource result = (DataSource)BeanUtils.instantiate(type);
      this.maybeGetDriverClassName();
      this.bind(result);
      return result;
   }

   private void maybeGetDriverClassName() {
      if(!this.properties.containsKey("driverClassName") && this.properties.containsKey("url")) {
         String url = (String)this.properties.get("url");
         String driverClass = DatabaseDriver.fromJdbcUrl(url).getDriverClassName();
         this.properties.put("driverClassName", driverClass);
      }

   }

   private void bind(DataSource result) {
      MutablePropertyValues properties = new MutablePropertyValues(this.properties);
      (new RelaxedDataBinder(result)).withAlias("url", new String[]{"jdbcUrl"}).bind(properties);
   }

   public DataSourceBuilder type(Class type) {
      this.type = type;
      return this;
   }

   public DataSourceBuilder url(String url) {
      this.properties.put("url", url);
      return this;
   }

   public DataSourceBuilder driverClassName(String driverClassName) {
      this.properties.put("driverClassName", driverClassName);
      return this;
   }

   public DataSourceBuilder username(String username) {
      this.properties.put("username", username);
      return this;
   }

   public DataSourceBuilder password(String password) {
      this.properties.put("password", password);
      return this;
   }

   public Class findType() {
      if(this.type != null) {
         return this.type;
      } else {
         for(String name : DATA_SOURCE_TYPE_NAMES) {
            try {
               return ClassUtils.forName(name, this.classLoader);
            } catch (Exception var6) {
               ;
            }
         }

         return null;
      }
   }

   private Class getType() {
      Class<? extends DataSource> type = this.findType();
      if(type != null) {
         return type;
      } else {
         throw new IllegalStateException("No supported DataSource type found");
      }
   }
}
