package io.terminus.galaxy.order.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.dao.OrderExtraDao;
import io.terminus.galaxy.order.model.OrderExtra;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Mail: F@terminus.io
 * Data: 16/3/11
 * Author: yangzefeng
 */
@Service @Slf4j
public class OrderExtraReadServiceImpl implements OrderExtraReadService {

    private final OrderExtraDao orderExtraDao;

    @Autowired
    public OrderExtraReadServiceImpl(OrderExtraDao orderExtraDao) {
        this.orderExtraDao = orderExtraDao;
    }

    @Override
    public Response<OrderExtra> findExtraByOrderIdAndType(Long orderId, Integer orderType) {
        try {
            OrderExtra orderExtra = orderExtraDao.findByOrderIdAndType(orderId, orderType);
            if (null == orderExtra) {
                log.error("fail to find order extra by order id {}, order type {}",
                        orderId, orderType);
                return Response.fail("order.extra.not.found");
            }
            return Response.ok(orderExtra);
        }catch (Exception e) {
            log.error("fail to find extra by order id {}, type {}, cause:{}",
                    orderId, orderType, Throwables.getStackTraceAsString(e));
            return Response.fail("order.extra.query.fail");
        }
    }
}
