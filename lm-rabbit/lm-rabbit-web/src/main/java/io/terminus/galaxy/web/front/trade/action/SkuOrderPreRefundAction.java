package io.terminus.galaxy.web.front.trade.action;

import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.order.model.OrderExtra;
import io.terminus.galaxy.order.service.OrderExtraReadService;
import io.terminus.parana.common.util.RespUtil;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.util.Params;
import io.terminus.parana.pay.enums.PayChannelBusinessType;
import io.terminus.parana.web.pay.service.PayWebService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.Map;

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
                orderId
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
