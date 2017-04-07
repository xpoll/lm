package cn.blmdz.rabbit.web.front.trade.action;

import java.io.Serializable;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.api.client.util.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.order.model.OrderExtra;
import cn.blmdz.rabbit.order.service.OrderExtraReadService;
import cn.blmdz.wolf.common.util.RespUtil;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.util.Params;
import cn.blmdz.wolf.pay.enums.PayChannelBusinessType;
import cn.blmdz.wolf.web.pay.service.PayWebService;

/**
 * Desc: 卖家同意退款
 * Mail: houly@terminus.io
 * author: Hou Luyao
 * Date: 16/3/9.
 */
@Component
public class SkuOrderPreRefundAction implements TradeAction<Boolean> {

    private final PayWebService payWebService;
    private final OrderReadService orderReadService;
    private final OrderExtraReadService orderExtraReadService;

    @Autowired
    public SkuOrderPreRefundAction(PayWebService payWebService, OrderReadService orderReadService, OrderExtraReadService orderExtraReadService) {
        this.payWebService = payWebService;
        this.orderReadService = orderReadService;
        this.orderExtraReadService = orderExtraReadService;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());

        //退款单
        Response<SkuOrderRefund> skuOrderRefundRe = orderReadService.findSkuOrderRefundById(orderId);
        SkuOrderRefund refund = RespUtil.orServerEx(
                skuOrderRefundRe,
                "findSkuOrderRefundById",
                new Long[]{orderId}
        );

        //子订单ID
        Long skuOrderId = refund.getParentId();

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

        //订单跟踪
        Response<OrderExtra> orderExtraRes = orderExtraReadService.findExtraByOrderIdAndType(shopOrder.getId(), OrderType.SHOP_ORDER.value());
        if(!orderExtraRes.isSuccess()){
            throw new ServiceException(orderExtraRes.getError());
        }
        OrderExtra orderExtra = orderExtraRes.getResult();
        String channel =orderExtra.getChannel();
        String tradeNo =orderExtra.getTradeNo();

        //发起退款请求
        Response<Boolean> refundRequest = payWebService.refundRequest(orderId,
                OrderType.SKU_ORDER_REFUND.value(),
                tradeNo,
                refund.getRefundAmount(),
                refund.getBuyerNote(),
                PayChannelBusinessType.REJECT_REFUND);
        if (!refundRequest.isSuccess()) {
            throw new ServiceException(refundRequest.getError());
        }

        return Boolean.TRUE;
    }
}
