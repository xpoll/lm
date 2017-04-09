package cn.blmdz.rabbit.web.core.trade.action;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.ImmutableSet;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.enums.OrderStatus;
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
        ShopOrder shopOrder = RespHelper.<ShopOrder>orServEx(orderReadService.findShopOrderById(orderId));
        // 子订单
        List<SkuOrder> skuOrders = RespHelper.<List<SkuOrder>>orServEx(orderReadService.findSkuOrderByParentId(orderId));

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
