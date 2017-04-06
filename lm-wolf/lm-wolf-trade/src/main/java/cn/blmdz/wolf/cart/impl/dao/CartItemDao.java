package cn.blmdz.wolf.cart.impl.dao;

import java.util.Collection;
import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.cart.model.CartItem;

@Repository
public class CartItemDao extends MyBatisDao {
   public Boolean deleteBy(CartItem cartItem) {
      return Boolean.valueOf(this.getSqlSession().delete(this.sqlId("deleteBy"), cartItem) >= 0);
   }

   public Boolean batchDeleteBySkuIds(Long buyerId, List skuIds) {
      return Boolean.valueOf(this.getSqlSession().delete(this.sqlId("batchDeleteBySkuIds"), ImmutableMap.of("buyerId", buyerId, "skuIds", skuIds)) >= 0);
   }

   public Integer countCartQuantity(Long id) {
      return (Integer)this.getSqlSession().selectOne(this.sqlId("countCartQuantity"), id);
   }

   public List listAllSkuId(Long buyerId) {
      return this.getSqlSession().selectList(this.sqlId("listAllSkuId"), buyerId);
   }

   public List loadsByBuyer(Collection skuIds, Long buyerId) {
      return this.getSqlSession().selectList(this.sqlId("loadsByBuyer"), ImmutableMap.of("skuIds", skuIds, "buyerId", buyerId));
   }
}
