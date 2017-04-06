/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package io.terminus.galaxy.order.dao.impl;

import io.terminus.galaxy.order.dao.BaseDaoTest;
import io.terminus.galaxy.order.dao.OrderExtraDao;
import io.terminus.galaxy.order.model.OrderExtra;
import io.terminus.common.model.Paging;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Arrays;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * Author  : panxin
 * Date    : 3:51 PM 3/11/16
 * Mail    : panxin@terminus.io
 */
public class OrderExtraDaoTest extends BaseDaoTest {

    @Autowired
    private OrderExtraDao orderExtraDao;

    private OrderExtra orderExtra;

    private OrderExtra createOne(Long id){
        orderExtra = new OrderExtra();
        // orderExtra.setId(id);
        orderExtra.setOrderId(id+10);
        orderExtra.setOrderType(1);
        if (id > 5) {
            orderExtra.setOrderType(2);
        }
        return orderExtra;
    }

    @Before
    public void before(){
        for (int i = 0; i < 10; i++) {
            OrderExtra model = createOne(Long.valueOf(i));
            orderExtraDao.create(model);
        }
    }

    @Test
    public void testAll(){
        testFindBys();
        testPaging();
        testUpdate();
    }

    /**
     * 测试根据ID, IDs, type查找
     */
    public void testFindBys() {
        // 通过ID查找
        OrderExtra model =  orderExtraDao.findById(1L);
        assertThat(model.getOrderId(), is(10L));

        // 通过IDs查找
        List<OrderExtra> moderList = orderExtraDao.findByIds(Arrays.asList(new Long[]{1L, 2L, 3L}));
        assertNotNull(moderList);

        // 通过orderId and orderType    excepted model = null
        model = orderExtraDao.findByOrderIdAndType(10L, 6);
        assertNull(model);

        // 通过orderId and orderType    excepted model != null
        model = orderExtraDao.findByOrderIdAndType(16L, 2);
        assertNotNull(model);
    }

    /**
     * 分页
     */
    public void testPaging(){
        Paging<OrderExtra> modelList = orderExtraDao.paging(1, 5);
        assertThat(modelList.isEmpty(), is(false));
    }

    /**
     * 更新
     */
    public void testUpdate() {
        OrderExtra model = new OrderExtra();
        model.setId(3L);
        model.setOrderId(1000L);
        Boolean result = orderExtraDao.update(model);
        assertThat(result, is(true));
    }

}
