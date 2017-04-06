package cn.blmdz.wolf.category.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.parana.category.model.ShopCategory;

@Repository
public class ShopCategoryDao extends MyBatisDao<ShopCategory> {
   public List findByShopId(Long shopId) {
      return this.getSqlSession().selectList(this.sqlId("findByShopId"), shopId);
   }

   public List<ShopCategory> findChildrenByShopIdAndPid(Long shopId, Long pid) {
      return this.getSqlSession().selectList(this.sqlId("findByShopIdAndPid"), ImmutableMap.of("shopId", shopId, "pid", pid));
   }
}
