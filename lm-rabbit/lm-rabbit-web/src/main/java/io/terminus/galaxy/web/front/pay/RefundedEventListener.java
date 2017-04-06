package io.terminus.galaxy.web.front.pay;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderAction;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.web.core.trade.engine.ActionRouter;
import io.terminus.parana.order.model.ShopOrderRefund;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.web.pay.event.RefundedEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

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
