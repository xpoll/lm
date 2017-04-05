package io.terminus.parana.category.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class ShopCategoryDao extends MyBatisDao {
   public List findByShopId(Long shopId) {
      return this.getSqlSession().selectList(this.sqlId("findByShopId"), shopId);
   }

   public List findChildrenByShopIdAndPid(Long shopId, Long pid) {
      return this.getSqlSession().selectList(this.sqlId("findByShopIdAndPid"), ImmutableMap.of("shopId", shopId, "pid", pid));
   }
}
