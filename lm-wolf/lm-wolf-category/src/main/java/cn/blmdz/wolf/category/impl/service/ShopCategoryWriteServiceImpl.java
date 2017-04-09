package cn.blmdz.wolf.category.impl.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryDao;
import cn.blmdz.wolf.category.impl.manager.ShopCategoryManager;
import cn.blmdz.wolf.category.model.ShopCategory;
import cn.blmdz.wolf.category.service.ShopCategoryWriteService;

@Service
public class ShopCategoryWriteServiceImpl implements ShopCategoryWriteService {
   private static final Logger log = LoggerFactory.getLogger(ShopCategoryWriteServiceImpl.class);
   private final ShopCategoryDao shopCategoryDao;
   private final ShopCategoryManager shopCategoryManager;

   @Autowired
   public ShopCategoryWriteServiceImpl(ShopCategoryDao shopCategoryDao, ShopCategoryManager shopCategoryManager) {
      this.shopCategoryDao = shopCategoryDao;
      this.shopCategoryManager = shopCategoryManager;
   }

   public Response create(ShopCategory shopCategory) {
      shopCategory.setHasChildren(Boolean.valueOf(false));
      shopCategory.setHasItem(Boolean.valueOf(false));
      Long shopId = shopCategory.getShopId();
      Long pid = (Long)MoreObjects.firstNonNull(shopCategory.getPid(), Long.valueOf(0L));
      shopCategory.setPid(pid);

      try {
         if(pid.longValue() > 0L) {
            ShopCategory parent = (ShopCategory)this.shopCategoryDao.findById(pid);
            if(parent == null) {
               log.error("no parent shop category(id={}) found", pid);
               return Response.fail("shop.category.parent.not.found");
            }

            shopCategory.setLevel(Integer.valueOf(parent.getLevel().intValue() + 1));
            parent.setHasChildren(Boolean.valueOf(true));
            if(parent.getHasItem().booleanValue()) {
               log.error("parent shop category(id={}) has item associated, it can not has children category", pid);
               return Response.fail("shop.category.parent.has.item");
            }

            for(ShopCategory child : this.shopCategoryDao.findChildrenByShopIdAndPid(shopId, pid)) {
               if(Objects.equal(child.getName(), shopCategory.getName())) {
                  log.error("duplicate shop category name:{} under parent category(id={})", shopCategory.getName(), pid);
                  return Response.fail("shop.category.name.duplicated");
               }
            }

            this.shopCategoryManager.create(shopCategory, parent);
         } else {
            shopCategory.setLevel(Integer.valueOf(1));
            this.shopCategoryDao.create(shopCategory);
         }

         return Response.ok(shopCategory);
      } catch (Exception var8) {
         log.error("failed to create {}, cause:{}", shopCategory, Throwables.getStackTraceAsString(var8));
         return Response.fail("category.create.fail");
      }
   }

   public Response updateName(Long shopCategoryId, Long shopId, String name) {
      try {
         ShopCategory shopCategory = (ShopCategory)this.shopCategoryDao.findById(shopCategoryId);
         if(shopCategory == null) {
            log.error("shop category(id={}) not found", shopCategoryId);
            return Response.fail("shop.category.not.found");
         } else {
            for(ShopCategory child : this.shopCategoryDao.findChildrenByShopIdAndPid(shopId, shopCategory.getPid())) {
               if(Objects.equal(child.getName(), name)) {
                  log.error("duplicate shop category name:{} under parent category(id={})", name, shopCategory.getPid());
                  return Response.fail("shop.category.name.duplicated");
               }
            }

            ShopCategory u = new ShopCategory();
            u.setId(shopCategoryId);
            u.setShopId(shopId);
            u.setName(name);
            this.shopCategoryDao.update(u);
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var8) {
         log.error("failed to update shop category(id={})\'s name ={}, cause:{}", new Object[]{shopCategoryId, name, Throwables.getStackTraceAsString(var8)});
         return Response.fail("shop.category.name.update.fail");
      }
   }

   public Response delete(Long shopCategoryId, Long shopId) {
      try {
         ShopCategory shopCategory = (ShopCategory)this.shopCategoryDao.findById(shopCategoryId);
         if(shopCategory == null) {
            log.error("shop category(id={}) not found", shopCategoryId);
            return Response.fail("shop.category.not.found");
         } else if(shopCategory.getHasChildren().booleanValue()) {
            log.error("shop category(id={}) not leaf", shopCategoryId);
            return Response.fail("shop.category.not.leaf");
         } else {
            ShopCategory parent = null;
            Long pid = shopCategory.getPid();
            if(pid.longValue() > 0L) {
               parent = (ShopCategory)this.shopCategoryDao.findById(pid);
               if(parent != null) {
                  List<ShopCategory> children = this.shopCategoryDao.findChildrenByShopIdAndPid(shopId, pid);
                  if(children.size() == 1) {
                     parent.setHasChildren(Boolean.FALSE);
                  }
               }
            }

            this.shopCategoryManager.delete(shopCategory, parent);
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var7) {
         log.error("failed to delete shop category(id={}) of shop(id={}), cause:{}", new Object[]{shopCategoryId, shopId, Throwables.getStackTraceAsString(var7)});
         return Response.fail("shop.category.delete.fail");
      }
   }

   public Response move(Long shopCategoryId, int direction) {
      try {
         ShopCategory sc = (ShopCategory)this.shopCategoryDao.findById(shopCategoryId);
         if(sc == null) {
            log.warn("shopCategory(id={}) not found, can not move direction={}", shopCategoryId, Integer.valueOf(direction));
            return Response.fail("shop.category.not.found");
         } else {
            List<ShopCategory> brothers = this.shopCategoryDao.findChildrenByShopIdAndPid(sc.getShopId(), sc.getPid());
            if(brothers.isEmpty()) {
               return Response.ok(Boolean.FALSE);
            } else {
               Collections.sort(brothers);
               Map<Long, Integer> toIndex = Maps.newHashMap();

               for(int i = 0; i < brothers.size(); ++i) {
                  ShopCategory b = (ShopCategory)brothers.get(i);
                  int idx = i + 1;
                  toIndex.put(b.getId(), Integer.valueOf(idx));
               }

               for(int i = 0; i < brothers.size(); ++i) {
                  ShopCategory b = (ShopCategory)brothers.get(i);
                  int idx = i + 1;
                  if(Objects.equal(b.getId(), shopCategoryId)) {
                     if(i + 1 < brothers.size() && direction == 1) {
                        ShopCategory n = (ShopCategory)brothers.get(i + 1);
                        toIndex.put(n.getId(), Integer.valueOf(idx));
                        toIndex.put(b.getId(), Integer.valueOf(idx + 1));
                     }

                     if(i > 0 && direction == -1) {
                        ShopCategory n = (ShopCategory)brothers.get(i - 1);
                        toIndex.put(n.getId(), Integer.valueOf(idx));
                        toIndex.put(b.getId(), Integer.valueOf(idx - 1));
                     }
                     break;
                  }
               }

               List<ShopCategory> toUpdates = Lists.newArrayList();

               for(int i = 0; i < brothers.size(); ++i) {
                  ShopCategory b = (ShopCategory)brothers.get(i);
                  int idx = ((Integer)toIndex.get(b.getId())).intValue();
                  if(!Objects.equal(b.getIndex(), Integer.valueOf(idx))) {
                     toUpdates.add(this.toUpdateIndex(b.getId(), Integer.valueOf(idx)));
                  }
               }

               if(!toUpdates.isEmpty()) {
                  this.shopCategoryManager.batchUpdate(toUpdates);
               }

               return Response.ok(Boolean.TRUE);
            }
         }
      } catch (Exception var10) {
         log.error("move shopCategory(id={}) failed, direction={}, cause:{}", new Object[]{shopCategoryId, Integer.valueOf(direction), Throwables.getStackTraceAsString(var10)});
         return Response.fail("shop.category.move.fail");
      }
   }

   public Response updateDisclosed(Long shopCategoryId, Boolean disclosed) {
      try {
         ShopCategory toUpdate = new ShopCategory();
         toUpdate.setId(shopCategoryId);
         toUpdate.setDisclosed(disclosed);
         this.shopCategoryDao.update(toUpdate);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("update shopCategory(id={}) failed, disclosed={}, cause:{}", new Object[]{shopCategoryId, disclosed, Throwables.getStackTraceAsString(var4)});
         return Response.fail("shop.category.update.fail");
      }
   }

   private ShopCategory toUpdateIndex(Long id, Integer idx) {
      ShopCategory c = new ShopCategory();
      c.setId(id);
      c.setIndex(idx);
      return c;
   }
}
