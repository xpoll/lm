package cn.blmdz.rabbit.order.dao.impl;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.rabbit.order.dao.BaseDaoTest;
import cn.blmdz.rabbit.order.dao.OrderFinishInfoDao;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;

import java.util.Date;

/**
 * Created by cuiwentao
 * on 16/3/23
 */
public class OrderFinishInfoDaoTest extends BaseDaoTest {

    @Autowired
    private OrderFinishInfoDao orderFinishInfoDao;
    /**
     * create a new OrderCloseInfo
     *
     * @param id
     * @return
     */
    private OrderFinishInfo createOne(Long id) {
        //OrderCloseInfo
        OrderFinishInfo orderFinishInfo = new OrderFinishInfo();
        orderFinishInfo.setId(id);
        orderFinishInfo.setOrderId(id);
        orderFinishInfo.setSellerId(id);
        orderFinishInfo.setSellerName("seller");
        orderFinishInfo.setShopId(id);
        orderFinishInfo.setShopName("shop");
        orderFinishInfo.setBuyerId(id);
        orderFinishInfo.setBuyerName("buyer");
        orderFinishInfo.setOrderType(1);
        orderFinishInfo.setType(1);
        orderFinishInfo.setPayType(1);
        orderFinishInfo.setOrderStatus(1);
        orderFinishInfo.setSystemNo("str");
        orderFinishInfo.setOriginFee(id);
        orderFinishInfo.setFee(id);
        orderFinishInfo.setDiscount(id);
        orderFinishInfo.setShipFee(id);
        orderFinishInfo.setShipFeeDiscount(id);
        orderFinishInfo.setIntegral(id);
        orderFinishInfo.setBalance(id);
        orderFinishInfo.setSaleTax(id);
        orderFinishInfo.setShipFeeSaleTax(id);
        orderFinishInfo.setIsSettlemented(Boolean.TRUE);

        orderFinishInfo.setCreatedAt(new Date());
        orderFinishInfo.setUpdatedAt(new Date());

        return orderFinishInfo;
    }



    @Test
    public void testUpdate(){
        OrderFinishInfo model = createOne(1L);
        model.setFee(100L);
        Boolean result = orderFinishInfoDao.update(model);
        Assert.assertTrue(model.getFee() == 100L);
    }
}
