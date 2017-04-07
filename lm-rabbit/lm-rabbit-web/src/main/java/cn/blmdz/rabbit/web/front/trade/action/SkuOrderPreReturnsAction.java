package cn.blmdz.rabbit.web.front.trade.action;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.rabbit.order.service.OrderExtraReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.web.pay.service.PayWebService;


/**
 * 卖家确认收到货物
 * 1、发起退款请求
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/10/16
 * Time: 6:56 PM
 */
@Component
public class SkuOrderPreReturnsAction extends SkuOrderPreRefundAction{

    @Autowired
    public SkuOrderPreReturnsAction(PayWebService payWebService,
                                    OrderReadService orderReadService,
                                    OrderExtraReadService orderExtraReadService) {
        super(payWebService, orderReadService, orderExtraReadService);
    }
}
