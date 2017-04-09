package cn.blmdz.wolf.item.impl.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.item.impl.dao.ItemSnapshotDao;
import cn.blmdz.wolf.item.model.ItemSnapshot;
import cn.blmdz.wolf.item.service.ItemSnapshotReadService;

@Service
public class ItemSnapshotReadServiceImpl implements ItemSnapshotReadService {
   private static final Logger log = LoggerFactory.getLogger(ItemSnapshotReadServiceImpl.class);
   private final ItemSnapshotDao itemSnapshotDao;

   @Autowired
   public ItemSnapshotReadServiceImpl(ItemSnapshotDao itemSnapshotDao) {
      this.itemSnapshotDao = itemSnapshotDao;
   }

   public Response findById(Long id) {
      try {
         ItemSnapshot itemSnapshot = (ItemSnapshot)this.itemSnapshotDao.findById(id);
         if(itemSnapshot == null) {
            log.error("item snapshot(id={}) not found", id);
            return Response.fail("snapshot.not.found");
         } else {
            return Response.ok(itemSnapshot);
         }
      } catch (Exception var3) {
         log.error("failed to find item snapshot(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("snapshot.find.fail");
      }
   }

   public Response findByItemIdAndItemInfoMd5(Long itemId, String itemInfoMd5) {
      try {
         ItemSnapshot itemSnapshot = this.itemSnapshotDao.findByItemIdAndItemInfoMd5(itemId, itemInfoMd5);
         return Response.ok(itemSnapshot);
      } catch (Exception var4) {
         log.error("failed to find item snapshot(itemId={}, itemInfoMd5={}), cause:{}", new Object[]{itemId, itemInfoMd5, Throwables.getStackTraceAsString(var4)});
         return Response.fail("snapshot.find.fail");
      }
   }
}
