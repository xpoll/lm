package cn.blmdz.wolf.web.core;

import java.util.concurrent.Executors;

import javax.servlet.Filter;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.web.filter.HiddenHttpMethodFilter;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.eventbus.AsyncEventBus;
import com.google.common.eventbus.EventBus;

import cn.blmdz.aide.file.ImageServer;
import cn.blmdz.aide.file.aliyun.AliyunImageServer;
import cn.blmdz.wolf.search.item.ItemSearchWriteService;
import cn.blmdz.wolf.web.core.auth.DefaultRoleAuthorizor;
import cn.blmdz.wolf.web.core.auth.DefaultTypeAuthorizor;
import cn.blmdz.wolf.web.core.auth.RoleAuthorizor;
import cn.blmdz.wolf.web.core.auth.TypeAuthorizor;
import cn.blmdz.wolf.web.core.events.item.listener.IndexListener;

@Configuration
@EnableWebMvc
@EnableAutoConfiguration
@ComponentScan
public class CoreWebConfiguration extends WebMvcConfigurerAdapter {
   @Bean
   public MessageSource messageSource() {
      ReloadableResourceBundleMessageSource messageSource = new ReloadableResourceBundleMessageSource();
      messageSource.setBasename("classpath:messages/messages");
      messageSource.setCacheSeconds(3600);
      messageSource.setUseCodeAsDefaultMessage(true);
      messageSource.setDefaultEncoding("UTF-8");
      return messageSource;
   }

   @Bean
   public Filter hiddenHttpMethodFilter() {
      return new HiddenHttpMethodFilter();
   }

   @Bean
   public ImageServer aliyunOSSImageServer(@Value("${oss.endpoint}") String endpoint, @Value("${oss.appKey}") String appKey, @Value("${oss.appSecret}") String appSecret, @Value("${oss.bucketName}") String bucketName) {
      return new AliyunImageServer(endpoint, appKey, appSecret, bucketName);
   }

   @Bean
   public EventBus eventBus() {
      return new AsyncEventBus(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors()));
   }

   @Bean
   public ObjectMapper nonNullObjectMapper() {
      ObjectMapper objectMapper = new ObjectMapper();
      objectMapper.setSerializationInclusion(Include.NON_NULL);
      return objectMapper;
   }

   @ConditionalOnBean({ItemSearchWriteService.class})
   @Bean
   public IndexListener indexListener(ItemSearchWriteService itemSearchWriteService, EventBus eventBus) {
      return new IndexListener(itemSearchWriteService, eventBus);
   }

   @Configuration
   @ConditionalOnMissingBean({RoleAuthorizor.class})
   protected static class RoleAuthorizorConfiguration {
      @Bean
      public RoleAuthorizor roleAuthorizor() {
         return new DefaultRoleAuthorizor();
      }
   }

   @ConditionalOnMissingBean({TypeAuthorizor.class})
   protected static class TypeAuthorizorConfiguration {
      @Bean
      public TypeAuthorizor typeAuthorizor() {
         return new DefaultTypeAuthorizor();
      }
   }
}
