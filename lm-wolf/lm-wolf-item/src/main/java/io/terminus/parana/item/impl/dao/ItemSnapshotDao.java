package io.terminus.parana.item.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.item.model.ItemSnapshot;
import org.springframework.stereotype.Repository;

@Repository
public class ItemSnapshotDao extends MyBatisDao {
   public ItemSnapshot findByItemIdAndItemInfoMd5(Long itemId, String itemInfoMd5) {
      return (ItemSnapshot)this.getSqlSession().selectOne(this.sqlId("findByItemIdAndItemInfoMd5"), ImmutableMap.of("itemId", itemId, "itemInfoMd5", itemInfoMd5));
   }
}
