package cn.blmdz.wolf.pay.impl.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.model.TradePay;

@Repository
public class TradePayDao extends MyBatisDao {
   public TradePay loadBySystemNo(String systemNo) {
      return (TradePay)this.getSqlSession().selectOne(this.sqlId("loadBySystemNo"), systemNo);
   }

   public TradePay loadByOrderIdsAndOrderType(String orderIds, Integer orderType) {
      return (TradePay)this.getSqlSession().selectOne(this.sqlId("loadByOrderIdsAndOrderType"), ImmutableMap.of("orderIds", orderIds, "orderType", orderType));
   }

   public TradePay loadByOrderIdAndOrderType(Long orderId, Integer orderType) {
      return (TradePay)this.getSqlSession().selectOne(this.sqlId("loadByOrderIdAndOrderType"), ImmutableMap.of("orderId", orderId, "orderType", orderType));
   }

   public boolean updateStatus(String systemNO, Integer status) {
      return this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("systemNo", systemNO, "status", status)) > 0;
   }
}
