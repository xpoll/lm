package io.terminus.galaxy.order.service;

import io.terminus.common.model.Response;
import io.terminus.galaxy.order.model.OrderExtra;

/**
 * Desc: orderTrack 查询接口
 * Mail: F@terminus.io
 * Data: 16/3/11
 * Author: yangzefeng
 */
public interface OrderExtraReadService {

    Response<OrderExtra> findExtraByOrderIdAndType(Long orderId, Integer orderType);
}
