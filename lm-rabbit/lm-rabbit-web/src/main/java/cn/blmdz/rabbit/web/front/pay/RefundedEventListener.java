package cn.blmdz.rabbit.web.front.pay;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.enums.OrderAction;
import cn.blmdz.rabbit.order.enums.OrderStatus;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.web.core.trade.engine.ActionRouter;
import cn.blmdz.wolf.order.model.ShopOrderRefund;
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.pay.model.PayChannel;
import cn.blmdz.wolf.web.pay.event.RefundedEvent;
import lombok.extern.slf4j.Slf4j;

/**
 * DATE: 16/5/23 下午2:21 <br>
 * MAIL: zhanghecheng@terminus.io <br>
 * AUTHOR: zhanghecheng
 */
@Slf4j
@Component
public class RefundedEventListener {

    private final EventBus eventBus;

    private final ActionRouter actionRouter;

    private final OrderReadService orderReadService;

    @Autowired
    public RefundedEventListener(EventBus eventBus, ActionRouter actionRouter, OrderReadService orderReadService) {
        this.eventBus = eventBus;
        this.actionRouter = actionRouter;
        this.orderReadService = orderReadService;
    }


    @PostConstruct
    public void register() {
        eventBus.register(this);
    }

    @Subscribe
    public void updateOrderRefunded(RefundedEvent refundedEvent){

        Long nid=0L;

        PayChannel payChannel=refundedEvent.getPayChannel();

        Response<SkuOrderRefund> refundRes = orderReadService.findSkuOrderRefundById(payChannel.getRefundOrderId());
        if(!refundRes.isSuccess()){
            throw new ServiceException(refundRes.getError());
        }

        if(payChannel.getOrderType().equals(OrderType.MERGE_ORDER_REFUND.value())){

            //todo merge order refund
        }

        if(payChannel.getOrderType().equals(OrderType.SHOP_ORDER_REFUND.value())){

            Response<ShopOrderRefund> shopOrderRefundRes = orderReadService.findShopOrderRefundById(payChannel.getRefundOrderId());
            if(!shopOrderRefundRes.isSuccess()){
                throw new ServiceException(shopOrderRefundRes.getError());
            }
            ShopOrderRefund refund = shopOrderRefundRes.getResult();
            nid = refund.getNodeInstanceId();

            //shop退款
            if(nid.equals(OrderStatus.APPLY_REFUND.value())){
                log.error("shop refund current not support");
                //actionRouter.updateOrder(OrderAction.SKU_ORDER_REFUND_CALLBACK.value(), updateParams);
            }

            //shop退货
            if(nid.equals(OrderStatus.DELIVE_RETURNS.value())){
                log.error("shop refund current not support");
                //actionRouter.updateOrder(OrderAction.SKU_ORDER_RETURNS_CALLBACK.value(), updateParams);
            }
        }

        if(payChannel.getOrderType().equals(OrderType.SKU_ORDER_REFUND.value())){

            Response<SkuOrderRefund> skuOrderRefundRes = orderReadService.findSkuOrderRefundById(payChannel.getRefundOrderId());
            if(!skuOrderRefundRes.isSuccess()){
                throw new ServiceException(skuOrderRefundRes.getError());
            }
            SkuOrderRefund refund = skuOrderRefundRes.getResult();
            nid = refund.getNodeInstanceId();

            //sku退款
            if(nid.equals(OrderStatus.SKU_REFUND_APPLY_REFUND.value())){
                actionRouter.updateOrder(OrderAction.SKU_ORDER_REFUND_CALLBACK.value(), refundedEvent.getParams());
            }

            //sku退货
            if(nid.equals(OrderStatus.SKU_REFUND_DELIVE_RETURNS.value())){
                actionRouter.updateOrder(OrderAction.SKU_ORDER_RETURNS_CALLBACK.value(), refundedEvent.getParams());
            }
        }
    }

}
