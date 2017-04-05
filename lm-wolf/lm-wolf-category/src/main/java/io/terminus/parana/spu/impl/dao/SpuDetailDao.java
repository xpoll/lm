package io.terminus.parana.spu.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.spu.model.SpuDetail;
import org.springframework.stereotype.Repository;

@Repository
public class SpuDetailDao extends MyBatisDao {
   public SpuDetail findBySpuId(Long spuId) {
      return (SpuDetail)this.getSqlSession().selectOne(this.sqlId("findBySpuId"), spuId);
   }

   public Integer deleteBySpuId(Long spuId) {
      return Integer.valueOf(this.getSqlSession().delete(this.sqlId("delete"), spuId));
   }
}
