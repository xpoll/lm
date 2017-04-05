package io.terminus.parana.item.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.common.utils.BeanMapper;
import io.terminus.parana.item.common.Digestors;
import io.terminus.parana.item.impl.dao.ItemSnapshotDao;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import io.terminus.parana.item.model.ItemDetail;
import io.terminus.parana.item.model.ItemSnapshot;
import io.terminus.parana.item.service.ItemSnapshotWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ItemSnapshotWriteServiceImpl implements ItemSnapshotWriteService {
   private static final Logger log = LoggerFactory.getLogger(ItemSnapshotWriteServiceImpl.class);
   private final ItemSnapshotDao itemSnapshotDao;

   @Autowired
   public ItemSnapshotWriteServiceImpl(ItemSnapshotDao itemSnapshotDao) {
      this.itemSnapshotDao = itemSnapshotDao;
   }

   public Response create(Item item, ItemDetail itemDetail, ItemAttribute itemAttribute) {
      try {
         ItemSnapshot itemSnapshot = new ItemSnapshot();
         BeanMapper.copy(item, itemSnapshot);
         BeanMapper.copy(itemDetail, itemSnapshot);
         BeanMapper.copy(itemAttribute, itemSnapshot);
         String itemInfoMd5 = Digestors.itemDigest(item, itemDetail, itemAttribute);
         itemSnapshot.setItemInfoMd5(itemInfoMd5);
         itemSnapshot.setItemId(item.getId());
         this.itemSnapshotDao.create(itemSnapshot);
         return Response.ok(itemSnapshot.getId());
      } catch (Exception var6) {
         log.error("failed to create snapshot for item (id={}), cause:{}", item.getId(), Throwables.getStackTraceAsString(var6));
         return Response.fail("snapshot.create.fail");
      }
   }
}
