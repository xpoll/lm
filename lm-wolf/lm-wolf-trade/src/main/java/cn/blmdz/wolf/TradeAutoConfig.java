package cn.blmdz.wolf;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.order.component.DefaultOrderComponent;
import cn.blmdz.wolf.order.component.OrderComponent;
import cn.blmdz.wolf.order.dao.MergeOrderDao;
import cn.blmdz.wolf.order.dao.MergeOrderRefundDao;
import cn.blmdz.wolf.order.dao.ShopOrderDao;
import cn.blmdz.wolf.order.dao.ShopOrderRefundDao;
import cn.blmdz.wolf.order.dao.SkuOrderDao;
import cn.blmdz.wolf.order.dao.SkuOrderRefundDao;

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
