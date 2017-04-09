package cn.blmdz.wolf;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;

import cn.blmdz.home.search.ESClient;
import cn.blmdz.wolf.cache.BackCategoryCacher;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryItemDao;
import cn.blmdz.wolf.search.impl.BaseItemQueryBuilder;
import cn.blmdz.wolf.search.impl.DefaultIndexedItemFactory;
import cn.blmdz.wolf.search.impl.DefaultItemQueryBuilder;
import cn.blmdz.wolf.search.impl.IndexedItemFactory;
import cn.blmdz.wolf.search.item.SearchItemProperties;

@Configuration
@ComponentScan({"cn.blmdz.wolf.item.impl", "cn.blmdz.wolf.category.impl", "cn.blmdz.wolf.brand.impl", "cn.blmdz.wolf.shop.impl", "cn.blmdz.wolf.spu.impl"})
public class ItemAutoConfig {
   @Bean
   public ObjectMapper nonNullObjectMapper() {
      ObjectMapper objectMapper = new ObjectMapper();
      objectMapper.setSerializationInclusion(Include.NON_NULL);
      return objectMapper;
   }

   @Configuration
   @EnableConfigurationProperties({SearchItemProperties.class})
   @ConditionalOnClass({ESClient.class})
   @ComponentScan({"cn.blmdz.home.search.api", "cn.blmdz.wolf.search.impl", "cn.blmdz.wolf.cache"})
   public static class ItemSearchConfiguration {
      @Bean
      public ESClient esClient(@Value("${search.host:localhost}") String host, @Value("${search.port:9200}") Integer port) {
         return new ESClient(host, port.intValue());
      }

      @Configuration
      @ConditionalOnMissingBean({IndexedItemFactory.class})
      protected static class IndexItemFactoryConfiguration {
         @Bean
         public IndexedItemFactory indexedItemFactory(BackCategoryCacher backCategoryCacher, ShopCategoryItemDao shopCategoryItemDao) {
            return new DefaultIndexedItemFactory(backCategoryCacher, shopCategoryItemDao);
         }
      }

      @Configuration
      @ConditionalOnMissingBean({BaseItemQueryBuilder.class})
      protected static class ItemQueryBuilderConfiguration {
         @Bean
         public BaseItemQueryBuilder itemQueryBuilder() {
            return new DefaultItemQueryBuilder();
         }
      }
   }
}
