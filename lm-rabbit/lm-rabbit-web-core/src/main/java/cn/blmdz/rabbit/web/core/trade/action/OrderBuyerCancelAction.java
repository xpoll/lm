package cn.blmdz.rabbit.web.core.trade.action;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.eventbus.EventBus;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.enums.OrderStatus;
import cn.blmdz.rabbit.web.core.trade.event.RollbakStockEvent;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.wolf.common.utils.RespHelper;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.Order;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.order.util.Params;

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
