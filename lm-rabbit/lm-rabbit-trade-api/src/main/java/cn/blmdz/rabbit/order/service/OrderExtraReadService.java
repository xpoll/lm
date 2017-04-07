package cn.blmdz.rabbit.order.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.model.OrderExtra;

/**
 * Desc: orderTrack 查询接口
 * Mail: F@terminus.io
 * Data: 16/3/11
 * Author: yangzefeng
 */
public interface OrderExtraReadService {

    Response<OrderExtra> findExtraByOrderIdAndType(Long orderId, Integer orderType);
}
