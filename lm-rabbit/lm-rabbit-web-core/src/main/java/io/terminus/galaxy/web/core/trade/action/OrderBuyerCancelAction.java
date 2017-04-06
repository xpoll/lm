package io.terminus.galaxy.web.core.trade.action;

import com.google.common.eventbus.EventBus;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.web.core.trade.event.RollbakStockEvent;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 店铺订单买家取消订单动作
 *
 * 在这个 action 里面, 会把 shopOrder 的状态流转到 {@link OrderStatus#BUYER_CANCEL},
 * skuOrder 的状态流转到 {@link OrderStatus#SKU_ORDER_CANCEL_BY_BUYER}
 *
 * @author Effet
 */
@Component
public class OrderBuyerCancelAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final EventBus eventBus;

    @Autowired
    public OrderBuyerCancelAction(OrderReadService orderReadService,
                                  OrderActionReadService orderActionReadService,
                                  OrderWriteService orderWriteService,
                                  EventBus eventBus) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
        this.eventBus = eventBus;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());

        // 订单
        ShopOrder shopOrder = RespHelper.orServEx(orderReadService.findShopOrderById(orderId));
        // 子订单
        List<SkuOrder> skuOrders = RespHelper.orServEx(orderReadService.findSkuOrderByParentId(orderId));

        List<Order> toUpdates = new ArrayList<>();
        // 流转到买家取消订单节点
        shopOrder.setNodeInstanceId(OrderStatus.BUYER_CANCEL.value());
        // 找到接下来可以执行的action
        shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
        toUpdates.add(shopOrder);

        for (SkuOrder skuOrder : skuOrders) {
            // sku订单对应的买家取消订单节点
            skuOrder.setNodeInstanceId(OrderStatus.SKU_ORDER_CANCEL_BY_BUYER.value());
            skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
            toUpdates.add(skuOrder);
        }

        //批量更新订单
        orderWriteService.update(toUpdates, context);

        Map<Long, Integer> skuIdAndQuantity = new HashMap<>();
        for (SkuOrder skuOrder : skuOrders) {
            skuIdAndQuantity.put(skuOrder.getSkuId(), skuOrder.getQuantity());
        }

        //publish stock change event
        RollbakStockEvent rollbakStockEvent = new RollbakStockEvent(skuIdAndQuantity);
        eventBus.post(rollbakStockEvent);

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
