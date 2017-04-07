package cn.blmdz.rabbit.settlement.impl.dao;


import static org.junit.Assert.assertEquals;

import java.util.Date;
import java.util.Map;

import org.joda.time.DateTime;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.rabbit.settlement.impl.dao.SettlementSumOfDailyDao;
import cn.blmdz.rabbit.settlement.model.SettlementSumOfDaily;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/22/16
 * Time: 9:29 PM
 */
public class SettlementSumOfDailyDaoTest extends BaseDaoTest{

    @Autowired
    private SettlementSumOfDailyDao settlementSumOfDailyDao;

    private SettlementSumOfDaily settlementSumOfDaily;


    @Test
    public void testCreate(){
        mock();
        Assert.assertNotNull(settlementSumOfDailyDao.create(settlementSumOfDaily));
    }

    @Test
    public void testPaging() throws Exception {
        int total = 10;
        for (int i=0; i<total; ++i) {
            settlementSumOfDailyDao.create(mock());
        }

        Map<String, Object> criteria = Maps.newHashMap();
        criteria.put("startAt", DateTime.now().withTimeAtStartOfDay().toDate());
        criteria.put("endAt", DateTime.now().withTimeAtStartOfDay().plusDays(1).toDate());
        Paging<SettlementSumOfDaily> sum = settlementSumOfDailyDao.paging(0, 8, criteria);
        assertEquals(10, sum.getTotal().intValue());
        assertEquals(8, sum.getData().size());
    }

    private SettlementSumOfDaily mock(){
        settlementSumOfDaily = new SettlementSumOfDaily();
        settlementSumOfDaily.setOrderCount(10);
        settlementSumOfDaily.setFee(300l);
        settlementSumOfDaily.setSellerDiscount(200l);
        settlementSumOfDaily.setRefundFee(100l);
        settlementSumOfDaily.setCommission(100l);
        settlementSumOfDaily.setIntegral(1l);
        settlementSumOfDaily.setThirdPartyFee(20l);
        settlementSumOfDaily.setReceivable(200l);
        settlementSumOfDaily.setPlatformDiscount(300l);
        settlementSumOfDaily.setOriginFee(300l);
        settlementSumOfDaily.setShipFee(0l);
        settlementSumOfDaily.setSumAt(new Date());
        settlementSumOfDaily.setCreatedAt(new Date());
        settlementSumOfDaily.setUpdatedAt(new Date());
        return settlementSumOfDaily;
    }
}