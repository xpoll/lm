package cn.blmdz.boot.datasource.autoconfigure;

import javax.sql.DataSource;
import javax.sql.XADataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactoryUtils;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.boot.autoconfigure.condition.ConditionOutcome;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SpringBootCondition;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.Order;
import org.springframework.core.type.AnnotatedTypeMetadata;
import org.springframework.jdbc.core.JdbcOperations;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

import cn.blmdz.boot.datasource.autoconfigure.DataSourceBuilder;
import cn.blmdz.boot.datasource.autoconfigure.DataSourceInitializer;
import cn.blmdz.boot.datasource.autoconfigure.DataSourceInitializerPostProcessor;
import cn.blmdz.boot.datasource.autoconfigure.EmbeddedDataSourceConfiguration;
import cn.blmdz.boot.datasource.autoconfigure.EmbeddedDatabaseConnection;
import cn.blmdz.boot.datasource.autoconfigure.metadata.DataSourcePoolMetadataProvidersConfiguration;
import cn.blmdz.boot.datasource.properties.DataSourceProperties;

@Configuration
@ConditionalOnClass({DataSource.class, EmbeddedDatabaseType.class})
@EnableConfigurationProperties({DataSourceProperties.class})
@Import({DataSourceInitializerPostProcessor.Registrar.class, DataSourcePoolMetadataProvidersConfiguration.class})
public class DataSourceAutoConfiguration {
   private static final Logger log = LoggerFactory.getLogger(DataSourceAutoConfiguration.class);

   public static boolean containsAutoConfiguredDataSource(ConfigurableListableBeanFactory beanFactory) {
      try {
         BeanDefinition beanDefinition = beanFactory.getBeanDefinition("dataSource");
         return EmbeddedDataSourceConfiguration.class.getName().equals(beanDefinition.getFactoryBeanName());
      } catch (NoSuchBeanDefinitionException var2) {
         return false;
      }
   }

   @Order(2147483637)
   static class DataSourceAvailableCondition extends SpringBootCondition {
      private final SpringBootCondition nonEmbedded = new DataSourceAutoConfiguration.NonEmbeddedDataSourceCondition();
      private final SpringBootCondition embeddedCondition = new DataSourceAutoConfiguration.EmbeddedDataSourceCondition();

      public ConditionOutcome getMatchOutcome(ConditionContext context, AnnotatedTypeMetadata metadata) {
         return !this.hasBean(context, DataSource.class) && !this.hasBean(context, XADataSource.class)?(this.anyMatches(context, metadata, new Condition[]{this.nonEmbedded, this.embeddedCondition})?ConditionOutcome.match("existing auto database detected"):ConditionOutcome.noMatch("no existing bean configured database")):ConditionOutcome.match("existing bean configured database detected");
      }

      private boolean hasBean(ConditionContext context, Class type) {
         return BeanFactoryUtils.beanNamesForTypeIncludingAncestors(context.getBeanFactory(), type, true, false).length > 0;
      }
   }

   @Configuration
   @ConditionalOnMissingBean({DataSourceInitializer.class})
   protected static class DataSourceInitializerConfiguration {
      @Bean
      public DataSourceInitializer dataSourceInitializer() {
         return new DataSourceInitializer();
      }
   }

   @Conditional({DataSourceAutoConfiguration.EmbeddedDataSourceCondition.class})
   @ConditionalOnMissingBean({DataSource.class, XADataSource.class})
   @Import({EmbeddedDataSourceConfiguration.class})
   protected static class EmbeddedConfiguration {
   }

   static class EmbeddedDataSourceCondition extends SpringBootCondition {
      private final SpringBootCondition nonEmbedded = new DataSourceAutoConfiguration.NonEmbeddedDataSourceCondition();

      public ConditionOutcome getMatchOutcome(ConditionContext context, AnnotatedTypeMetadata metadata) {
         if(this.anyMatches(context, metadata, new Condition[]{this.nonEmbedded})) {
            return ConditionOutcome.noMatch("existing non-embedded database detected");
         } else {
            EmbeddedDatabaseType type = EmbeddedDatabaseConnection.get(context.getClassLoader()).getType();
            return type == null?ConditionOutcome.noMatch("no embedded database detected"):ConditionOutcome.match("embedded database " + type + " detected");
         }
      }
   }

   @Configuration
   @Conditional({DataSourceAutoConfiguration.DataSourceAvailableCondition.class})
   protected static class JdbcTemplateConfiguration {
      @Autowired(
         required = false
      )
      private DataSource dataSource;

      @Bean
      @ConditionalOnMissingBean({JdbcOperations.class})
      public JdbcTemplate jdbcTemplate() {
         return new JdbcTemplate(this.dataSource);
      }

      @Bean
      @ConditionalOnMissingBean({NamedParameterJdbcOperations.class})
      public NamedParameterJdbcTemplate namedParameterJdbcTemplate() {
         return new NamedParameterJdbcTemplate(this.dataSource);
      }
   }

   @Conditional({DataSourceAutoConfiguration.NonEmbeddedDataSourceCondition.class})
   @ConditionalOnMissingBean({DataSource.class, XADataSource.class})
   protected static class NonEmbeddedConfiguration {
      @Autowired
      private DataSourceProperties properties;

      @Bean
      @ConfigurationProperties(
         prefix = "datasource"
      )
      public DataSource dataSource() {
         DataSourceBuilder factory = DataSourceBuilder.create(this.properties.getClassLoader()).driverClassName(this.properties.getDriverClassName()).url(this.properties.getUrl()).username(this.properties.getUsername()).password(this.properties.getPassword());
         if(this.properties.getType() != null) {
            factory.type(this.properties.getType());
         }

         return factory.build();
      }
   }

   static class NonEmbeddedDataSourceCondition extends SpringBootCondition {
      public ConditionOutcome getMatchOutcome(ConditionContext context, AnnotatedTypeMetadata metadata) {
         return this.getDataSourceClassLoader(context) != null?ConditionOutcome.match("supported DataSource class found"):ConditionOutcome.noMatch("missing supported DataSource");
      }

      private ClassLoader getDataSourceClassLoader(ConditionContext context) {
         Class<?> dataSourceClass = (new DataSourceBuilder(context.getClassLoader())).findType();
         return dataSourceClass == null?null:dataSourceClass.getClassLoader();
      }
   }
}
