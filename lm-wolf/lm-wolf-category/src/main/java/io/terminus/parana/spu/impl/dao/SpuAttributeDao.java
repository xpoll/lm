package io.terminus.parana.spu.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.spu.model.SpuAttribute;
import org.springframework.stereotype.Repository;

@Repository
public class SpuAttributeDao extends MyBatisDao {
   public SpuAttribute findBySpuId(Long spuId) {
      return (SpuAttribute)this.getSqlSession().selectOne(this.sqlId("findBySpuId"), spuId);
   }

   public boolean updateBySpuId(SpuAttribute spuAttribute) {
      return this.getSqlSession().update(this.sqlId("update"), spuAttribute) == 1;
   }

   public boolean deleteBySpuId(Long spuId) {
      return this.getSqlSession().delete(this.sqlId("delete"), spuId) == 1;
   }
}
