package cn.blmdz.wolf.item.impl.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.item.common.Digestors;
import cn.blmdz.wolf.item.impl.dao.ItemSnapshotDao;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.model.ItemAttribute;
import cn.blmdz.wolf.item.model.ItemDetail;
import cn.blmdz.wolf.item.model.ItemSnapshot;
import cn.blmdz.wolf.item.service.ItemSnapshotWriteService;

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
