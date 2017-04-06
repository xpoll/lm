package io.terminus.galaxy.order.dao.impl;

import io.terminus.galaxy.order.dao.BaseDaoTest;
import io.terminus.galaxy.order.dao.OrderMoneyFlowDao;
import io.terminus.galaxy.order.model.OrderMoneyFlow;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Date;

/**
 * Created by cuiwentao
 * on 16/3/23
 */
public class OrderMoneyFlowDaoTest extends BaseDaoTest {


    @Autowired
    private OrderMoneyFlowDao orderMoneyFlowDao;
    /**
     * create a new OrderBalanceInfo
     *
     * @param id
     * @return
     */
    private OrderMoneyFlow createOne(Long id) {
        //OrderBalanceInfo
        OrderMoneyFlow orderMoneyFlow = new OrderMoneyFlow();
        orderMoneyFlow.setId(id);
        orderMoneyFlow.setOrderId(id);
        orderMoneyFlow.setOrderType(1);
        orderMoneyFlow.setSellerId(id);
        orderMoneyFlow.setSellerName("seller");
        orderMoneyFlow.setShopId(id);
        orderMoneyFlow.setShopName("shop");
        orderMoneyFlow.setBuyerId(id);
        orderMoneyFlow.setBuyerName("buyer");
        orderMoneyFlow.setType(1);
        orderMoneyFlow.setPayType(1);
        orderMoneyFlow.setChannel("1");
        orderMoneyFlow.setSystemNo("1");
        orderMoneyFlow.setTradeNo("1");
        orderMoneyFlow.setPaymentCode("pay");
        orderMoneyFlow.setBatchNo("1");
        orderMoneyFlow.setFee(1L);
        orderMoneyFlow.setMemo("memo");
        orderMoneyFlow.setIsSettlemented(Boolean.TRUE);

        orderMoneyFlow.setCreatedAt(new Date());
        orderMoneyFlow.setUpdatedAt(new Date());

        return orderMoneyFlow;
    }


    @Test
    public void testUpdate(){
        OrderMoneyFlow model = createOne(1L);
        model.setFee(100L);
        Boolean result = orderMoneyFlowDao.update(model);
        Assert.assertTrue(model.getFee() == 100L);
    }
}
