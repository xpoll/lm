package io.terminus.parana.brand.impl.dao;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.brand.model.Brand;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class BrandDao extends MyBatisDao {
   public Brand findByName(String name) {
      return (Brand)this.getSqlSession().selectOne(this.sqlId("findByName"), name.toLowerCase());
   }

   public List findByNamePrefix(String namePrefix, Integer limit) {
      Integer size = (Integer)MoreObjects.firstNonNull(limit, Integer.valueOf(10));
      return this.getSqlSession().selectList(this.sqlId("findByNamePrefix"), ImmutableMap.of("name", namePrefix.toLowerCase(), "limit", size));
   }

   public boolean updateStatus(Long id, Integer status) {
      return this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", id, "status", status)) == 1;
   }
}
