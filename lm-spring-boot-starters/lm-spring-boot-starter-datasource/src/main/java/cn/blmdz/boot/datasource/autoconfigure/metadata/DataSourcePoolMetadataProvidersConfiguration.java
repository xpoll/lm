package cn.blmdz.boot.datasource.autoconfigure.metadata;

import cn.blmdz.boot.datasource.autoconfigure.metadata.CommonsDbcpDataSourcePoolMetadata;
import cn.blmdz.boot.datasource.autoconfigure.metadata.DataSourcePoolMetadata;
import cn.blmdz.boot.datasource.autoconfigure.metadata.DataSourcePoolMetadataProvider;
import cn.blmdz.boot.datasource.autoconfigure.metadata.HikariDataSourcePoolMetadata;

import com.zaxxer.hikari.HikariDataSource;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class DataSourcePoolMetadataProvidersConfiguration {
   @Configuration
   @ConditionalOnClass({BasicDataSource.class})
   static class CommonsDbcpPoolDataSourceMetadataProviderConfiguration {
      @Bean
      public DataSourcePoolMetadataProvider commonsDbcpPoolDataSourceMetadataProvider() {
         return new DataSourcePoolMetadataProvider() {
            public DataSourcePoolMetadata getDataSourcePoolMetadata(DataSource dataSource) {
               return dataSource instanceof BasicDataSource?new CommonsDbcpDataSourcePoolMetadata((BasicDataSource)dataSource):null;
            }
         };
      }
   }

   @Configuration
   @ConditionalOnClass({HikariDataSource.class})
   static class HikariPoolDataSourceMetadataProviderConfiguration {
      @Bean
      public DataSourcePoolMetadataProvider hikariPoolDataSourceMetadataProvider() {
         return new DataSourcePoolMetadataProvider() {
            public DataSourcePoolMetadata getDataSourcePoolMetadata(DataSource dataSource) {
               return dataSource instanceof HikariDataSource?new HikariDataSourcePoolMetadata((HikariDataSource)dataSource):null;
            }
         };
      }
   }
}
