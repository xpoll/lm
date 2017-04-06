package cn.blmdz.wolf.settlement.impl.dao;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.settlement.dto.SettlementSumOfDailyDto;
import cn.blmdz.wolf.settlement.model.OrderSettlement;

@Repository
public class OrderSettlementDao extends MyBatisDao {
   public List findBySellerId(Long sellerId) {
      return this.getSqlSession().selectList(this.sqlId("findBySellerId"), sellerId);
   }

   public List findByShopId(Long shopId) {
      return this.getSqlSession().selectList(this.sqlId("findByShopId"), shopId);
   }

   public OrderSettlement findByOrderId(Long orderId) {
      return (OrderSettlement)this.getSqlSession().selectOne(this.sqlId("findByOrderId"), orderId);
   }

   public Paging pagingByIsChecked(Integer offset, Integer limit, Boolean isChecked) {
      Long count = (Long)this.getSqlSession().selectOne(this.sqlId("countByIsChecked"), ImmutableMap.of("isChecked", isChecked));
      if(count.longValue() == 0L) {
         return new Paging(Long.valueOf(0L), Collections.emptyList());
      } else {
         List<OrderSettlement> orderTracks = this.getSqlSession().selectList(this.sqlId("pagingByIsChecked"), ImmutableMap.of("isChecked", isChecked, "offset", offset, "limit", limit));
         return new Paging(count, orderTracks);
      }
   }

   public List generateSettlementSumOfShopDaily(Date startAt, Date endAt) {
      return this.getSqlSession().selectList(this.sqlId("sumSellerSettlement"), ImmutableMap.of("startAt", startAt, "endAt", endAt));
   }

   public SettlementSumOfDailyDto generateSettlementSumOfPlatformDaily(Date startAt, Date endAt) {
      return (SettlementSumOfDailyDto)this.getSqlSession().selectOne(this.sqlId("sumPlatformSettlement"), ImmutableMap.of("startAt", startAt, "endAt", endAt));
   }

   public List generateSettlementSumOfShopDailyByPayType(Date startAt, Date endAt, Integer payType) {
      return this.getSqlSession().selectList(this.sqlId("sumSellerSettlementByPayType"), ImmutableMap.of("startAt", startAt, "endAt", endAt, "payType", payType));
   }

   public SettlementSumOfDailyDto generateSettlementSumOfPlatformDailyByPayType(Date startAt, Date endAt, Integer payType) {
      return (SettlementSumOfDailyDto)this.getSqlSession().selectOne(this.sqlId("sumPlatformSettlementByPayType"), ImmutableMap.of("startAt", startAt, "endAt", endAt, "payType", payType));
   }
}
