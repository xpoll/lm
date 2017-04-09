package cn.blmdz.wolf.spu.impl.dao;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.spu.model.SpuAttribute;

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
