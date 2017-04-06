package cn.blmdz.wolf.item.impl.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.parana.item.model.ItemSnapshot;

@Repository
public class ItemSnapshotDao extends MyBatisDao {
   public ItemSnapshot findByItemIdAndItemInfoMd5(Long itemId, String itemInfoMd5) {
      return (ItemSnapshot)this.getSqlSession().selectOne(this.sqlId("findByItemIdAndItemInfoMd5"), ImmutableMap.of("itemId", itemId, "itemInfoMd5", itemInfoMd5));
   }
}
