package cn.blmdz.rabbit.order;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.rabbit.order.component.GalaxyOrderComponent;
import cn.blmdz.rabbit.order.dao.OrderExtraDao;
import cn.blmdz.rabbit.order.dao.OrderFinishInfoDao;
import cn.blmdz.rabbit.order.dao.OrderMoneyFlowDao;
import cn.blmdz.wolf.TradeAutoConfig;
import cn.blmdz.wolf.order.component.OrderComponent;
import cn.blmdz.wolf.order.dao.MergeOrderDao;
import cn.blmdz.wolf.order.dao.MergeOrderRefundDao;
import cn.blmdz.wolf.order.dao.ShopOrderDao;
import cn.blmdz.wolf.order.dao.ShopOrderRefundDao;
import cn.blmdz.wolf.order.dao.SkuOrderDao;
import cn.blmdz.wolf.order.dao.SkuOrderRefundDao;

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
