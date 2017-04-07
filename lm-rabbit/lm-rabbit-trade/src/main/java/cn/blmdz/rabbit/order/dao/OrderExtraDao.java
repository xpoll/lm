/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.order.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.order.model.OrderExtra;


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
