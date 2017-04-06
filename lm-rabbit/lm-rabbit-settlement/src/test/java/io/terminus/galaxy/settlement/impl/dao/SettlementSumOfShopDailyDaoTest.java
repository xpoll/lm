package io.terminus.galaxy.settlement.impl.dao;

import io.terminus.galaxy.settlement.model.SettlementSumOfShopDaily;
import org.joda.time.DateTime;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Date;

/**
 * 店铺结算每日汇总DaoTest
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 14/12/23
 * Time: 上午9:17
 */
public class SettlementSumOfShopDailyDaoTest extends BaseDaoTest {

    @Autowired
    private SettlementSumOfShopDailyDao settlementSumOfShopDailyDao;

    private SettlementSumOfShopDaily settlementSumOfShopDaily;

    @Test
    public void testCreate(){
        mock();
        Assert.assertNotNull(settlementSumOfShopDailyDao.create(settlementSumOfShopDaily));
    }

    @Test
    public void testLoad(){
        testCreate();
        Assert.assertNotNull(settlementSumOfShopDailyDao.findById(settlementSumOfShopDaily.getId()));

    }

    @Test
    public void findBySumAt(){

        testCreate();
        SettlementSumOfShopDaily settlementSumOfDaily = settlementSumOfShopDailyDao.findBySumAt(settlementSumOfShopDaily.getShopId(),new DateTime().minusHours(1).withTimeAtStartOfDay().toDate());
        Assert.assertNotNull(settlementSumOfDaily);
    }


    private SettlementSumOfShopDaily mock(){
        settlementSumOfShopDaily = new SettlementSumOfShopDaily();
        settlementSumOfShopDaily.setSellerId(2l);
        settlementSumOfShopDaily.setSellerName("duandian");
        settlementSumOfShopDaily.setShopId(3l);
        settlementSumOfShopDaily.setShopName("端点科技");
        settlementSumOfShopDaily.setOrderCount(200);
        settlementSumOfShopDaily.setFee(200l);
        settlementSumOfShopDaily.setOriginFee(900l);
        settlementSumOfShopDaily.setRefundFee(200l);
        settlementSumOfShopDaily.setCommission(200l);
        settlementSumOfShopDaily.setIntegral(200l);
        settlementSumOfShopDaily.setThirdPartyFee(100l);
        settlementSumOfShopDaily.setPlatformDiscount(1010l);
        settlementSumOfShopDaily.setSellerDiscount(200l);
        settlementSumOfShopDaily.setReceivable(300l);
        settlementSumOfShopDaily.setShipFee(0l);
        settlementSumOfShopDaily.setCreatedAt(new Date());
        settlementSumOfShopDaily.setUpdatedAt(new Date());
        settlementSumOfShopDaily.setSumAt(new DateTime().minusHours(1).withTimeAtStartOfDay().toDate());
        return settlementSumOfShopDaily;

    }

}
