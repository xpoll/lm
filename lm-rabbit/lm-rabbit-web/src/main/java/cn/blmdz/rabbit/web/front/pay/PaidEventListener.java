package cn.blmdz.rabbit.web.front.pay;
import java.util.List;
import java.util.Map;

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
import cn.blmdz.wolf.order.model.MergeOrder;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.pay.enums.ExceptionPayType;
import cn.blmdz.wolf.pay.model.PayChannel;
import cn.blmdz.wolf.pay.model.TradePay;
import cn.blmdz.wolf.pay.service.PayWriteService;
import cn.blmdz.wolf.web.pay.event.PaidEvent;
import lombok.extern.slf4j.Slf4j;

/**
 * DATE: 16/5/23 下午2:21 <br>
 * MAIL: zhanghecheng@terminus.io <br>
 * AUTHOR: zhanghecheng
 */
@Slf4j
@Component
public class PaidEventListener {

    private final EventBus eventBus;

    private final OrderReadService orderReadService;

    private final PayWriteService payWriteService;

    private final ActionRouter actionRouter;

    @Autowired
    public PaidEventListener(EventBus eventBus, OrderReadService orderReadService, PayWriteService payWriteService, ActionRouter actionRouter) {
        this.eventBus = eventBus;
        this.orderReadService = orderReadService;
        this.payWriteService = payWriteService;
        this.actionRouter = actionRouter;
    }

    @PostConstruct
    public void register() {
        eventBus.register(this);
    }

    @Subscribe
    public void updateOrderPaid(PaidEvent paidEvent){
        //判断订单状态
        List<Long> orderIds=paidEvent.getOrderIds();
        TradePay tradePay=paidEvent.getTradePay();
        PayChannel payChannel=paidEvent.getPayChannel();
        Map<String, Object> updateParams=paidEvent.getParams();

        if(!checkOrderStatus(orderIds,tradePay.getOrderType())){
            //订单已关闭 创建异常记录
            Response<Boolean> exceptionPayRes = payWriteService.createExceptionPayTracks(payChannel.getTradeNo(), ExceptionPayType.TIME_OUT);
            if (!exceptionPayRes.isSuccess()) {
                log.error("create exception pay tracks where trade no: {}，exception type: {} fail,error:{}", payChannel.getTradeNo(), ExceptionPayType.TIME_OUT.value(), exceptionPayRes.getError());
                throw new ServiceException(exceptionPayRes.getError());
            }
        }
        //更新订单状态 不同的订单类型对应不同的支付成功action shopOrder
        if(tradePay.getOrderType().equals(OrderType.SHOP_ORDER.value())){
            for(Long orderId : orderIds){
                updateParams.put("orderId", orderId);
                Response<Map<String, Object>> updateR =
                        actionRouter.updateOrder(OrderAction.PAID.value(), updateParams);
                if (!updateR.isSuccess()) {
                    log.error("update order where id：{} fail,error: {}",orderId,updateR.getError());
                }
            }
        }
    }


    //判断订单状态
    private Boolean checkOrderStatus(List<Long> orderIds, Integer orderType){

        for(Long orderId : orderIds){

            if(orderType.equals(OrderType.MERGE_ORDER.value())){
                Response<MergeOrder> mergeOrderRes = orderReadService.findMergeOrderById(orderId);
                if(!mergeOrderRes.isSuccess()){
                    throw new ServiceException(mergeOrderRes.getError());
                }
                //todo 判断订单状态  merge 订单暂时没有状态

            }

            if(orderType.equals(OrderType.SHOP_ORDER.value())){

                Response<ShopOrder> shopOrderRes = orderReadService.findShopOrderById(orderId);
                if(!shopOrderRes.isSuccess()){
                    throw new ServiceException(shopOrderRes.getError());
                }
                Long shopOrderNId = shopOrderRes.getResult().getNodeInstanceId();
                Long buyerCancelNo = OrderStatus.BUYER_CANCEL.value();
                Long sellerCancelNo = OrderStatus.SELLER_CANCEL.value();
                if(shopOrderNId.equals(buyerCancelNo)||shopOrderNId.equals(sellerCancelNo)){
                    return Boolean.FALSE;
                }
            }
        }
        return Boolean.TRUE;
    }

}
