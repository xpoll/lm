package cn.blmdz.boot.datasource.autoconfigure;

import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.AbstractTransactionManagementConfiguration;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@ConditionalOnClass({JdbcTemplate.class, PlatformTransactionManager.class})
@AutoConfigureOrder(Integer.MAX_VALUE)
public class DataSourceTransactionManagerAutoConfiguration {
   @Autowired(
      required = false
   )
   private DataSource dataSource;

   @Bean
   @ConditionalOnMissingBean({PlatformTransactionManager.class})
   @ConditionalOnBean({DataSource.class})
   public DataSourceTransactionManager transactionManager() {
      return new DataSourceTransactionManager(this.dataSource);
   }

   @ConditionalOnMissingBean({AbstractTransactionManagementConfiguration.class})
   @Configuration
   @EnableTransactionManagement
   protected static class TransactionManagementConfiguration {
   }
}
