package io.terminus.galaxy.web.core.trade.action;

import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.ShopOrderRefund;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.service.OrderActionReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.service.OrderWriteService;
import io.terminus.parana.order.util.Params;
import lombok.extern.slf4j.Slf4j;
import org.dozer.DozerBeanMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: 用户申请退款,整单退款
 * Mail: houly@terminus.io
 * author: Hou Luyao
 * Date: 16/3/9.
 */
@Component
@Slf4j
public class OrderApplyRefundAction implements TradeAction<Boolean> {
    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final static DozerBeanMapper MAPPER = new DozerBeanMapper();

    @Autowired
    public OrderApplyRefundAction(OrderReadService orderReadService,
                          OrderActionReadService orderActionReadService,
                          OrderWriteService orderWriteService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());
        String buyerNote = Params.get(context, "buyerNote");
        Integer refundAmount = Params.get(context, "refundAmount");

        Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(orderId);
        if (!shopOrderR.isSuccess()) {
            throw new ServiceException(shopOrderR.getError());
        }

        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(orderId);
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }

        ShopOrder shopOrder = shopOrderR.getResult();
        List<SkuOrder> skuOrders = skuOrdersR.getResult();

        //创建退款单
        ShopOrderRefund shopOrderRefund = initShopOrderRefund(shopOrder, buyerNote, refundAmount);
        orderWriteService.create(shopOrderRefund, null);

        //设置订单和子订单,主订单和子订单状态同时往下流转
        List<Order> orders = updateShopOrderAndSkuOrders(shopOrder, skuOrders);

        //更新订单子订单
        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    /**
     * 初始化退款单
     */
    private ShopOrderRefund initShopOrderRefund(ShopOrder shopOrder, String buyerNote, Integer refundAmount){
        ShopOrderRefund shopOrderRefund = MAPPER.map(shopOrder, ShopOrderRefund.class);
        shopOrderRefund.setBuyerNote(buyerNote);
        shopOrderRefund.setRefundAmount(refundAmount);
        shopOrderRefund.setParentId(shopOrder.getId());
        shopOrderRefund.setType(OrderType.SHOP_ORDER_REFUND.value());
        //流转到买家申请退款接口
        shopOrderRefund.setNodeInstanceId(OrderStatus.APPLY_REFUND.value());
        //找到接下来可以执行的action
        shopOrderRefund.setNextActionInstanceIds(getNextActions(shopOrderRefund.getNodeInstanceId()));
        return shopOrderRefund;
    }

    /**
     * 设置订单和子订单,主订单和子订单状态同时往下流转
     */
    private List<Order> updateShopOrderAndSkuOrders(ShopOrder shopOrder, List<SkuOrder> skuOrders){
        List<Order> orders = Lists.newArrayList();
        //此时主订单处于等待卖家发货,下一步操作的操作只有等待卖家发货
        String nextAction = getNextActions(shopOrder.getNodeInstanceId(), OrderStatus.WAIT_FOR_DELIVE.value());
        shopOrder.setNextActionInstanceIds(nextAction);
        shopOrder.setNodeInstanceId(OrderStatus.APPLY_REFUND.value());
        orders.add(shopOrder);
        for(SkuOrder skuOrder : skuOrders){
            skuOrder.setNextActionInstanceIds(nextAction);
            skuOrder.setNodeInstanceId(OrderStatus.APPLY_REFUND.value());
            orders.add(skuOrder);
        }
        return orders;
    }
    /**
     * 获取下一步的action列表
     */
    private String getNextActions(Long nid, Long... aidsRemoved) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid, aidsRemoved);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }
}
