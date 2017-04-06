package cn.blmdz.wolf.pay.mock.impl.dao;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Repository;

import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.mock.model.MockedAlipayTrans;
import cn.blmdz.wolf.pay.mock.utils.Dates;

@Repository
public class MockedAlipayTransDao extends MyBatisDao {
   private static final String NAMESPACE = "MockedAlipayTrans.";

   public Long createIsNotExist(MockedAlipayTrans alipayTrans) {
      MockedAlipayTrans criteria = new MockedAlipayTrans();
      criteria.setIwAccountLogId(alipayTrans.getIwAccountLogId());
      MockedAlipayTrans existed = this.getBy(criteria);
      if(existed != null) {
         return existed.getId();
      } else {
         this.getSqlSession().insert("MockedAlipayTrans.create", alipayTrans);
         return alipayTrans.getId();
      }
   }

   public MockedAlipayTrans getBy(MockedAlipayTrans criteria) {
      return (MockedAlipayTrans)this.getSqlSession().selectOne("MockedAlipayTrans.getBy", criteria);
   }

   public List findByTradeNo(String paymentCode) {
      return this.getSqlSession().selectList("MockedAlipayTrans.findByTradeNo", paymentCode);
   }

   public List findByMerchantNo(String merchantOuterTradeNo) {
      return this.getSqlSession().selectList("MockedAlipayTrans.findByMerchantNo", merchantOuterTradeNo);
   }

   public List list(String tradeNo) {
      Map<String, Object> params = Maps.newHashMapWithExpectedSize(20);
      MockedAlipayTrans criteria = new MockedAlipayTrans();
      criteria.setTradeNo(tradeNo);
      params.put("criteria", criteria);
      return this.getSqlSession().selectList("MockedAlipayTrans.findBy", params);
   }

   public Paging findBy(MockedAlipayTrans criteria, Integer offset, Integer limit) {
      Map<String, Object> params = Maps.newHashMapWithExpectedSize(20);
      params.put("criteria", criteria);
      params.put("offset", offset);
      params.put("limit", limit);
      if(criteria.getCreatedAt() != null) {
         Date createdAt = criteria.getCreatedAt();
         params.put("createdStartAt", Dates.startOfDay(createdAt));
         params.put("createdEndAt", Dates.endOfDay(createdAt));
      }

      return this.findBy(params);
   }

   public Paging findBy(Map params) {
      Long total = (Long)this.getSqlSession().selectOne("MockedAlipayTrans.countOf", params);
      if(total.longValue() == 0L) {
         return new Paging(Long.valueOf(0L), Collections.emptyList());
      } else {
         List<MockedAlipayTrans> transes = this.getSqlSession().selectList("MockedAlipayTrans.findBy", params);
         return new Paging(total, transes);
      }
   }
}
