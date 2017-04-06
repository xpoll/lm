package io.terminus.galaxy.web.core.trade.action;

import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderAction;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderActionReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.service.OrderWriteService;
import io.terminus.parana.order.util.Params;
import org.dozer.DozerBeanMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: 子订单申请退款
 * 在这个action里面, 会创建skuOrderRefund, 状态为SKU_REFUND_APPLY_REFUND
 * skuOrder的状态不变, 同时动作 SKU_ORDER_APPLY_REFUND 需要隐藏
 * shopOrder状态不变, 同时动作 DELIVE 需要隐藏
 * @see io.terminus.galaxy.order.enums.OrderStatus 订单状态
 * @see io.terminus.galaxy.order.enums.OrderAction 订单操作
 * Mail: houly@terminus.io
 * author: Hou Luyao, yangzefeng
 * Date: 16/3/9.
 */
@Component
public class SkuOrderApplyRefundAction implements TradeAction<Boolean> {
    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final static DozerBeanMapper MAPPER = new DozerBeanMapper();

    @Autowired
    public SkuOrderApplyRefundAction(OrderReadService orderReadService,
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

        Response<SkuOrder> skuOrderR = orderReadService.findSkuOrderById(orderId);
        if (!skuOrderR.isSuccess()) {
            throw new ServiceException(skuOrderR.getError());
        }
        SkuOrder skuOrder = skuOrderR.getResult();
        Long skuOrderId = skuOrder.getParentId();

        Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(skuOrderId);
        if (!shopOrderR.isSuccess()) {
            throw new ServiceException(shopOrderR.getError());
        }

        ShopOrder shopOrder = shopOrderR.getResult();

        //创建退款单
        SkuOrderRefund skuOrderRefund = initSkuOrderRefund(skuOrder, buyerNote, refundAmount);
        orderWriteService.create(skuOrderRefund, null);

        //设置订单和子订单,子订单,主订单状态不流转
        List<Order> orders = updateShopOrderAndSkuOrders(shopOrder, skuOrder);

        //更新订单子订单
        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    private SkuOrderRefund initSkuOrderRefund(SkuOrder skuOrder, String buyerNote, Integer refundAmount){
        SkuOrderRefund skuOrderRefund = MAPPER.map(skuOrder, SkuOrderRefund.class);
        skuOrderRefund.setBuyerNote(buyerNote);
        skuOrderRefund.setRefundAmount(refundAmount);
        skuOrderRefund.setParentId(skuOrder.getId());
        skuOrderRefund.setType(OrderType.SKU_ORDER_REFUND.value());
        //流转到买家申请退款接口
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_APPLY_REFUND.value());
        //找到接下来可以执行的action
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        return skuOrderRefund;
    }

    /**
     * 设置订单和子订单,子订单状态同时往下流转,主订单状态不流转
     * @param shopOrder
     * @param skuOrder
     * @return
     */
    private List<Order> updateShopOrderAndSkuOrders(ShopOrder shopOrder, SkuOrder skuOrder){
        List<Order> orders = Lists.newArrayList();
        //主订单的发货动作需要隐藏
        String nextAction = getNextActions(shopOrder.getNodeInstanceId(), OrderAction.DELIVE.value());
        shopOrder.setNextActionInstanceIds(nextAction);
        orders.add(shopOrder);
        //子订单申请退款的动作需要隐藏
        skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId(), OrderAction.SKU_ORDER_APPLY_REFUND.value()));
        skuOrder.setHasRefund(1);
        orders.add(skuOrder);
        return orders;
    }

    /**
     * 获取下一步的aciton列表
     * @param nid
     * @return
     */
    private String getNextActions(Long nid, Long... excludeActionIds) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid, excludeActionIds);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }

}
