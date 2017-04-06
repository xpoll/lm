package io.terminus.galaxy.web.trade.controller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.dto.OrderDetail;
import io.terminus.galaxy.order.dto.PreOrder;
import io.terminus.galaxy.order.dto.PrePay;
import io.terminus.galaxy.order.dto.RichShopOrder;
import io.terminus.galaxy.web.core.trade.service.GalaxyOrderReadService;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.common.model.ParanaUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Mail: F@terminus.io
 * Data: 16/3/7
 * Author: yangzefeng
 */
@RestController
@Slf4j
@RequestMapping("/api/order")
public class ReadOrders {

    private final GalaxyOrderReadService galaxyOrderReadService;

    @Autowired
    public ReadOrders(GalaxyOrderReadService galaxyOrderReadService) {
        this.galaxyOrderReadService = galaxyOrderReadService;
    }

    @RequestMapping(value = "/pre-view", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public List<PreOrder> preView(@RequestParam("skus") String skus) {
        Response<List<PreOrder>> preOrderR =
                galaxyOrderReadService.preOrder(skus);
        if (!preOrderR.isSuccess()) {
            throw new JsonResponseException(preOrderR.getError());
        }

        return preOrderR.getResult();
    }

    @RequestMapping(value = "/pre-pay", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public PrePay prePay(@RequestParam("orderIds") String orderIds) {
        BaseUser baseUser = UserUtil.getCurrentUser();
        Response<PrePay> prePayR = galaxyOrderReadService.prePay(baseUser, orderIds);
        if (!prePayR.isSuccess()) {
            throw new JsonResponseException(prePayR.getError());
        }

        return prePayR.getResult();
    }

    @RequestMapping(value = "/{orderId}/detail", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public OrderDetail orderDetail(@PathVariable("orderId") Long orderId) {
        ParanaUser baseUser = UserUtil.getCurrentUser();
        Response<OrderDetail> orderDetailR = galaxyOrderReadService.orderDetail(baseUser, orderId);
        if (!orderDetailR.isSuccess()) {
            throw new JsonResponseException(orderDetailR.getError());
        }

        return orderDetailR.getResult();
    }

    @RequestMapping(value = "/buyer-list", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Paging<RichShopOrder> buyerOrderList(@RequestParam(value = "shopName", required = false) String shopName,
                                                @RequestParam(value = "orderId", required = false) Long orderId,
                                                @RequestParam(value = "nids", required = false) String nids,
                                                @RequestParam(value = "startAt", required = false) String startAt,
                                                @RequestParam(value = "endAt", required = false) String endAt,
                                                @RequestParam(value = "pageNo", required = false) Integer pageNo,
                                                @RequestParam(value = "size", required = false) Integer size) {
        BaseUser baseUser = UserUtil.getCurrentUser();
        Response<Paging<RichShopOrder>> richShopOrderR = galaxyOrderReadService.buyerOrderList(baseUser,
                shopName,
                orderId,
                nids,
                startAt, endAt,
                pageNo, size);
        if (!richShopOrderR.isSuccess()) {
            throw new JsonResponseException(richShopOrderR.getError());
        }

        return richShopOrderR.getResult();
    }
}
