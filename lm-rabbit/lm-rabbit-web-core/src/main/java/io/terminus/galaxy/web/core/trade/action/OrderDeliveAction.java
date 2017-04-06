package io.terminus.galaxy.web.core.trade.action;

import com.google.common.collect.ImmutableSet;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.common.utils.RespHelper;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.service.OrderActionReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.service.OrderWriteService;
import io.terminus.parana.order.util.Params;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 店铺订单卖家发货动作
 *
 * 在这个 action 里面, 会把 shopOrder 的状态流转到 {@link OrderStatus#DELIVED},
 * skuOrder 的状态流转到 {@link OrderStatus#SKU_ORDER_DELIVED}
 *
 * @author Effet
 */
@Component
public class OrderDeliveAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final static ImmutableSet<Long> SKIP_STATUS = ImmutableSet.of(
            OrderStatus.SKU_ORDER_CANCEL_BY_REFUND_CAUSE_BY_SKU_REFUND.value()
    );

    @Autowired
    public OrderDeliveAction(OrderReadService orderReadService,
                             OrderActionReadService orderActionReadService,
                             OrderWriteService orderWriteService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());

        // 订单
        ShopOrder shopOrder = RespHelper.orServEx(orderReadService.findShopOrderById(orderId));
        // 子订单
        List<SkuOrder> skuOrders = RespHelper.orServEx(orderReadService.findSkuOrderByParentId(orderId));

        List<Order> toUpdates = new ArrayList<>();
        // 流转到卖家已发货节点
        shopOrder.setNodeInstanceId(OrderStatus.DELIVED.value());
        // 找到接下来可以执行的action
        shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
        toUpdates.add(shopOrder);

        for (SkuOrder skuOrder : skuOrders) {
            //如果是已经被取消的子订单,不需要更新
            if (SKIP_STATUS.contains(skuOrder.getNodeInstanceId())) {
                continue;
            }
            // sku订单对应的卖家已发货节点
            skuOrder.setNodeInstanceId(OrderStatus.SKU_ORDER_DELIVED.value());
            skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
            toUpdates.add(skuOrder);
        }

        //批量更新订单
        orderWriteService.update(toUpdates, context);

        return Boolean.TRUE;
    }

    private String getNextActions(Long nid) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }
}
