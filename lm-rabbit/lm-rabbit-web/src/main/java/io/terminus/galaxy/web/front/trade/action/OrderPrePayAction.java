package io.terminus.galaxy.web.front.trade.action;

import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Splitters;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.lib.pay.channel.alipay.dto.RedirectInfo;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.web.pay.service.PayWebService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import static io.terminus.lib.pay.channel.wechatpay.request.WxRequest.verify;

/**
 * 订单预支付
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/9/16
 * Time: 8:28 PM
 */
@Slf4j @Component
public class OrderPrePayAction implements TradeAction<RedirectInfo> {

    private final PayWebService payWebService;

    private final OrderReadService orderReadService;

    @Autowired
    public OrderPrePayAction(PayWebService payWebService, OrderReadService orderReadService) {
        this.payWebService = payWebService;
        this.orderReadService = orderReadService;
    }


    //b2b2c中发生支付的订单是mergeOrder或shopOrder
    @Override
    public RedirectInfo execute(Map<String, Serializable> map) {
        //检查参数有效性
        checkParam(map);

        String orderIds = map.get("orderIds").toString();
        String channel = map.get("channel").toString();
        List<Long> ids = Splitters.splitToLong(orderIds, Splitters.COMMA);
        Long buyerId;
        String subject;
        Integer fee =0;

        Response<List<ShopOrder>> shopOrderRes = orderReadService.findShopOrderByIds(ids);
        if(!shopOrderRes.isSuccess()){
            throw new ServiceException(shopOrderRes.getError());
        }
        if(shopOrderRes.getResult().size()!=ids.size()){
            throw new ServiceException("prepay.action.param.order.ids.invalid");
        }
        buyerId = shopOrderRes.getResult().get(0).getBuyerId();
        subject="shop order pay";
        for (ShopOrder order: shopOrderRes.getResult()){
            fee+=order.getFee();
        }

        return payWebService.payRequest(ids, OrderType.SHOP_ORDER.value(), channel, fee, subject, buyerId);

    }

    private void checkParam(Map<String,Serializable> map){
        if(!map.containsKey("orderIds")){
            throw new ServiceException("prepay.action.param.order.ids.invalid");
        }
        if(!map.containsKey("channel")){
            throw new ServiceException("prepay.action.param.channel.invalid");

        }
    }


}
