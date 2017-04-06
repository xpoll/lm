package io.terminus.galaxy.web.front.trade.action;

import io.terminus.galaxy.order.service.OrderExtraReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.web.pay.service.PayWebService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


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
