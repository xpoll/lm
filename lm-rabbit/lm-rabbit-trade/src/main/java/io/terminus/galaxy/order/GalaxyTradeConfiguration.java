package io.terminus.galaxy.order;

import io.terminus.galaxy.order.component.GalaxyOrderComponent;
import io.terminus.galaxy.order.dao.OrderExtraDao;
import io.terminus.galaxy.order.dao.OrderFinishInfoDao;
import io.terminus.galaxy.order.dao.OrderMoneyFlowDao;
import io.terminus.parana.TradeAutoConfig;
import io.terminus.parana.order.component.OrderComponent;
import io.terminus.parana.order.dao.MergeOrderDao;
import io.terminus.parana.order.dao.MergeOrderRefundDao;
import io.terminus.parana.order.dao.ShopOrderDao;
import io.terminus.parana.order.dao.ShopOrderRefundDao;
import io.terminus.parana.order.dao.SkuOrderDao;
import io.terminus.parana.order.dao.SkuOrderRefundDao;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Mail: F@terminus.io
 * Data: 16/3/7
 * Author: yangzefeng
 */
@Configuration
@ComponentScan({"io.terminus.galaxy.order"})
@Import({TradeAutoConfig.class})
public class GalaxyTradeConfiguration {

    @Bean
    public OrderComponent orderComponent(MergeOrderDao mergeOrderDao,
                                         ShopOrderDao shopOrderDao,
                                         SkuOrderDao skuOrderDao,
                                         MergeOrderRefundDao mergeOrderRefundDao,
                                         ShopOrderRefundDao shopOrderRefundDao,
                                         SkuOrderRefundDao skuOrderRefundDao,
                                         OrderExtraDao orderExtraDao,
                                         OrderMoneyFlowDao orderMoneyFlowDao,
                                         OrderFinishInfoDao orderFinishInfoDao) {
        return new GalaxyOrderComponent(mergeOrderDao, shopOrderDao, skuOrderDao,
                mergeOrderRefundDao, shopOrderRefundDao, skuOrderRefundDao, orderExtraDao,orderMoneyFlowDao,orderFinishInfoDao);
    }
}
