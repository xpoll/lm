package cn.blmdz.wolf.spu.impl.dao;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.spu.model.SpuDetail;

@Repository
public class SpuDetailDao extends MyBatisDao {
   public SpuDetail findBySpuId(Long spuId) {
      return (SpuDetail)this.getSqlSession().selectOne(this.sqlId("findBySpuId"), spuId);
   }

   public Integer deleteBySpuId(Long spuId) {
      return Integer.valueOf(this.getSqlSession().delete(this.sqlId("delete"), spuId));
   }
}
