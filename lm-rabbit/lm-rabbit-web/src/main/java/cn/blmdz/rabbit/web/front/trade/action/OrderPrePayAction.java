package cn.blmdz.rabbit.web.front.trade.action;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.aide.pay.channel.alipay.dto.RedirectInfo;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.web.pay.service.PayWebService;
import lombok.extern.slf4j.Slf4j;

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
