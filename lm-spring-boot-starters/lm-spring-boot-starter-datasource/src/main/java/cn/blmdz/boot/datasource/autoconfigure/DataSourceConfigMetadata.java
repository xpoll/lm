package cn.blmdz.boot.datasource.autoconfigure;

import cn.blmdz.boot.datasource.autoconfigure.DataSourceBuilder;

import com.zaxxer.hikari.HikariDataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.tomcat.jdbc.pool.DataSource;
import org.springframework.boot.context.properties.ConfigurationProperties;

public class DataSourceConfigMetadata {
   @ConfigurationProperties("datasource")
   public DataSource tomcatDataSource() {
      return (DataSource)DataSourceBuilder.create().type(DataSource.class).build();
   }

   @ConfigurationProperties("datasource")
   public HikariDataSource hikariDataSource() {
      return (HikariDataSource)DataSourceBuilder.create().type(HikariDataSource.class).build();
   }

   @ConfigurationProperties("datasource")
   public BasicDataSource dbcpDataSource() {
      return (BasicDataSource)DataSourceBuilder.create().type(BasicDataSource.class).build();
   }
}
