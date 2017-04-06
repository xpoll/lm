package cn.blmdz.wolf.settlement.impl.dao;

import java.util.Collections;
import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.settlement.model.AfterSalesSettlement;

@Repository
public class AfterSalesSettlementDao extends MyBatisDao {
   public List findBySellerId(Long sellerId) {
      return this.getSqlSession().selectList(this.sqlId("findBySellerId"), sellerId);
   }

   public List findByShopId(Long shopId) {
      return this.getSqlSession().selectList(this.sqlId("findByShopId"), shopId);
   }

   public Paging pagingNeedCheck(Integer offset, Integer limit) {
      Long count = (Long)this.getSqlSession().selectOne(this.sqlId("countNeedCheck"), ImmutableMap.of("isChecked", Boolean.FALSE));
      if(count.longValue() == 0L) {
         return new Paging(Long.valueOf(0L), Collections.emptyList());
      } else {
         List<AfterSalesSettlement> orderTracks = this.getSqlSession().selectList(this.sqlId("pagingNeedCheck"), ImmutableMap.of("isChecked", Boolean.FALSE, "offset", offset, "limit", limit));
         return new Paging(count, orderTracks);
      }
   }
}
