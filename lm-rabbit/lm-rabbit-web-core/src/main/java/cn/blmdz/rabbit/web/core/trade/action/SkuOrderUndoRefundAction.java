package cn.blmdz.rabbit.web.core.trade.action;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.enums.OrderStatus;
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
 * Desc: 买家撤销sku退款单退款
 * 在这个action里面,skuOrder状态不变, 把申请退款的操作加回来
 * skuOrderRefund状态流转到 SKU_REFUND_UNDO,
 * shopOrder的需要做比较复杂的判断:
 * 如果其他的skuOrderRefund状态为 SKU_REFUND_REFUSE_REFUND | SKU_REFUND_CANCEL_BY_REFUND | SKU_REFUND_UNDO
 * shopOrder的 DELIVE 操作就应该加回来, 否则,shopOrder就不需要更新
 * @see cn.blmdz.rabbit.order.enums.OrderStatus 订单状态
 * @see cn.blmdz.rabbit.order.enums.OrderAction 订单操作
 * Mail: houly@terminus.io
 * author: Hou Luyao, yangzefeng
 * Date: 16/3/9.
 */
@Component
public class SkuOrderUndoRefundAction implements TradeAction<Boolean> {
    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final static Set<Long> JUDGE_STATUS = Sets.newHashSet(
            OrderStatus.SKU_REFUND_REFUSE_REFUND.value(),
            OrderStatus.SKU_REFUND_CANCEL_BY_REFUND.value(),
            OrderStatus.SKU_REFUND_UNDO.value()
    );

    @Autowired
    public SkuOrderUndoRefundAction(OrderReadService orderReadService,
                                            OrderActionReadService orderActionReadService,
                                            OrderWriteService orderWriteService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());

        List<Order> orders = Lists.newArrayList();

        Response<SkuOrderRefund> skuOrderRefundR = orderReadService.findSkuOrderRefundById(orderId);
        if (!skuOrderRefundR.isSuccess()) {
            throw new ServiceException(skuOrderRefundR.getError());
        }
        //子订单退款单
        SkuOrderRefund skuOrderRefund = skuOrderRefundR.getResult();

        //子订单ID
        Long skuOrderId = skuOrderRefund.getParentId();

        //子订单
        Response<SkuOrder> skuOrderR = orderReadService.findSkuOrderById(skuOrderId);
        if (!skuOrderR.isSuccess()) {
            throw new ServiceException(skuOrderR.getError());
        }
        SkuOrder skuOrder = skuOrderR.getResult();
        //订单ID
        Long shopOrderId = skuOrder.getParentId();
        //订单
        Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(shopOrderId);
        if (!shopOrderR.isSuccess()) {
            throw new ServiceException(shopOrderR.getError());
        }

        ShopOrder shopOrder = shopOrderR.getResult();

        //更新子订单退款单
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_UNDO.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        orders.add(skuOrderRefund);

        //更新子订单可操作action
        skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
        orders.add(skuOrder);

        //更新主订单的操作集合
        setShopOrderNextActions(shopOrder, skuOrderRefund.getId());
        orders.add(shopOrder);

        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    private void setShopOrderNextActions(ShopOrder shopOrder, Long currentSkuOrderRefundId) {
        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(shopOrder.getId());
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }
        List<SkuOrder> skuOrders = skuOrdersR.getResult();
        List<Long> skuOrderIds = new ArrayList<>();
        skuOrderIds.addAll(Lists.transform(skuOrders, new Function<SkuOrder, Long>() {
            @Override
            public Long apply(SkuOrder input) {
                return input.getId();
            }
        }));
        Response<List<SkuOrderRefund>> skuOrderRefundsR = orderReadService.findSkuOrderRefundByParentIds(skuOrderIds);
        if (!skuOrderRefundsR.isSuccess()) {
            throw new ServiceException(skuOrderRefundsR.getError());
        }
        List<SkuOrderRefund> refunds = skuOrderRefundsR.getResult();

        boolean canDeliver = true;
        for (SkuOrderRefund refund : refunds) {
            if (Objects.equals(currentSkuOrderRefundId, refund.getId())) {
                continue;
            }
            if (!JUDGE_STATUS.contains(refund.getNodeInstanceId())) {
                canDeliver = false;
                break;
            }
        }
        if (canDeliver) {
            //把DELIVE 这个操作加回来
            shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
        }
    }

    /**
     * 获取下一步的action列表
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
