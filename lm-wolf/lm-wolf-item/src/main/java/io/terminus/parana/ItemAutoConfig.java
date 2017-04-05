package io.terminus.parana;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.terminus.parana.cache.BackCategoryCacher;
import io.terminus.parana.category.impl.dao.ShopCategoryItemDao;
import io.terminus.parana.search.impl.BaseItemQueryBuilder;
import io.terminus.parana.search.impl.DefaultIndexedItemFactory;
import io.terminus.parana.search.impl.DefaultItemQueryBuilder;
import io.terminus.parana.search.impl.IndexedItemFactory;
import io.terminus.parana.search.item.SearchItemProperties;
import io.terminus.search.core.ESClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.item.impl", "io.terminus.parana.category.impl", "io.terminus.parana.brand.impl", "io.terminus.parana.shop.impl", "io.terminus.parana.spu.impl"})
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
   @ComponentScan({"io.terminus.search.api", "io.terminus.parana.search.impl", "io.terminus.parana.cache"})
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
