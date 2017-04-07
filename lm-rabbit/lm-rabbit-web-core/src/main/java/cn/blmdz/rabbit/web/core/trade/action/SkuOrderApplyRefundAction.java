package cn.blmdz.rabbit.web.core.trade.action;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.dozer.DozerBeanMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.enums.OrderAction;
import cn.blmdz.rabbit.order.enums.OrderStatus;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.Order;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.order.util.Params;

/**
 * Desc: 子订单申请退款
 * 在这个action里面, 会创建skuOrderRefund, 状态为SKU_REFUND_APPLY_REFUND
 * skuOrder的状态不变, 同时动作 SKU_ORDER_APPLY_REFUND 需要隐藏
 * shopOrder状态不变, 同时动作 DELIVE 需要隐藏
 * @see cn.blmdz.rabbit.order.enums.OrderStatus 订单状态
 * @see cn.blmdz.rabbit.order.enums.OrderAction 订单操作
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
        String buyerNote = (String) Params.get(context, "buyerNote");
        Integer refundAmount = (Integer) Params.get(context, "refundAmount");

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
