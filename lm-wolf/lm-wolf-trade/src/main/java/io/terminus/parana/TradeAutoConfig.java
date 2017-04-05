package io.terminus.parana;

import io.terminus.parana.order.component.DefaultOrderComponent;
import io.terminus.parana.order.component.OrderComponent;
import io.terminus.parana.order.dao.MergeOrderDao;
import io.terminus.parana.order.dao.MergeOrderRefundDao;
import io.terminus.parana.order.dao.ShopOrderDao;
import io.terminus.parana.order.dao.ShopOrderRefundDao;
import io.terminus.parana.order.dao.SkuOrderDao;
import io.terminus.parana.order.dao.SkuOrderRefundDao;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.order"})
public class TradeAutoConfig {
   @ConditionalOnMissingBean({OrderComponent.class})
   @Configuration
   public static class OrderComponentConfiguration {
      @Bean
      public OrderComponent orderComponent(MergeOrderDao mergeOrderDao, ShopOrderDao shopOrderDao, SkuOrderDao skuOrderDao, MergeOrderRefundDao mergeOrderRefundDao, ShopOrderRefundDao shopOrderRefundDao, SkuOrderRefundDao skuOrderRefundDao) {
         return new DefaultOrderComponent(mergeOrderDao, shopOrderDao, skuOrderDao, mergeOrderRefundDao, shopOrderRefundDao, skuOrderRefundDao);
      }
   }

   @Configuration
   @ComponentScan({"io.terminus.parana.cart"})
   public static class ShoppingCartConfiguration {
   }
}
