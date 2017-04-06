/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package io.terminus.galaxy.order.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.order.model.OrderExtra;
import org.springframework.stereotype.Repository;


/**
 * 订单相关信息DAO
 *
 * Author  : panxin
 * Date    : 2:20 PM 3/11/16
 * Mail    : panxin@terminus.io
 */
@Repository
public class OrderExtraDao extends MyBatisDao<OrderExtra>{

    public OrderExtra findByOrderIdAndType(Long orderId, Integer type) {
        return getSqlSession().selectOne(sqlId("findByOrderIdAndType"), ImmutableMap.of(
                "orderId", orderId, "type", type));
    }

}
