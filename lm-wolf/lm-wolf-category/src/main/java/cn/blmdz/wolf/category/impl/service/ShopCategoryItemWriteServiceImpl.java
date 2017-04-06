package cn.blmdz.wolf.category.impl.service;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryItemDao;
import cn.blmdz.wolf.parana.category.model.ShopCategoryItem;
import cn.blmdz.wolf.parana.category.service.ShopCategoryItemWriteService;

@Service
public class ShopCategoryItemWriteServiceImpl implements ShopCategoryItemWriteService {
   private static final Logger log = LoggerFactory.getLogger(ShopCategoryItemWriteServiceImpl.class);
   private final ShopCategoryItemDao shopCategoryItemDao;

   @Autowired
   public ShopCategoryItemWriteServiceImpl(ShopCategoryItemDao shopCategoryItemDao) {
      this.shopCategoryItemDao = shopCategoryItemDao;
   }

   public Response create(ShopCategoryItem shopCategoryItem) {
      return null;
   }

   public Response batchCreate(Long shopId, List<Long> shopCategoryIds, List<Long> itemIds) {
      try {
         Multimap<Long, ShopCategoryItem> bindMap = this.buildBindMap(shopId, itemIds);
         List<ShopCategoryItem> toCreates = Lists.newArrayList();

         for(Long itemId : itemIds) {
            Collection<ShopCategoryItem> existBinds = bindMap.get(itemId);
            Set<Long> existCategoryIds = Sets.newHashSet();

            for(ShopCategoryItem existBind : existBinds) {
               Long categoryId = existBind.getShopCategoryId();
               if(categoryId != null && categoryId.longValue() > 0L) {
                  existCategoryIds.add(categoryId);
               }
            }

            for(Long shopCategoryId : shopCategoryIds) {
               if(!existCategoryIds.contains(shopCategoryId)) {
                  ShopCategoryItem i = new ShopCategoryItem();
                  i.setShopId(shopId);
                  i.setItemId(itemId);
                  i.setShopCategoryId(shopCategoryId);
                  toCreates.add(i);
               }
            }
         }

         if(!toCreates.isEmpty()) {
            this.shopCategoryItemDao.creates(toCreates);
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var13) {
         log.error("batch create shop category item failed, shopId={}, shopCategoryIds={}, itemIds={}, cause:{}", new Object[]{shopId, shopCategoryIds, itemIds, Throwables.getStackTraceAsString(var13)});
         return Response.fail("shop.category.item.create.fail");
      }
   }

   public Response delete(Long shopId, Long shopCategoryId, Long itemId) {
      return this.batchDelete(shopId, Lists.newArrayList(new Long[]{shopCategoryId}), Lists.newArrayList(new Long[]{itemId}));
   }

   public Response batchDelete(Long shopId, List<Long> shopCategoryIds, List<Long> itemIds) {
      try {
         Multimap<Long, ShopCategoryItem> bindMap = this.buildBindMap(shopId, itemIds);
         List<Long> toDeletes = Lists.newArrayList();

         for(Long itemId : itemIds) {
            for(ShopCategoryItem existBind : bindMap.get(itemId)) {
               Long categoryId = existBind.getShopCategoryId();
               if(categoryId != null && categoryId.longValue() > 0L && shopCategoryIds.contains(categoryId)) {
                  toDeletes.add(existBind.getId());
               }
            }
         }

         if(!toDeletes.isEmpty()) {
            this.shopCategoryItemDao.deletes(toDeletes);
         }

         return Response.ok(Boolean.TRUE);
      } catch (Exception var12) {
         log.error("batch delete shop category item failed, shopId={}, shopCategoryIds={}, itemIds={}, cause:{}", new Object[]{shopId, shopCategoryIds, itemIds, Throwables.getStackTraceAsString(var12)});
         return Response.fail("shop.category.item.delete.fail");
      }
   }

   private Multimap buildBindMap(Long shopId, List itemIds) {
      List<ShopCategoryItem> binds = this.shopCategoryItemDao.findByShopIdAndItemIds(shopId, itemIds);
      Multimap<Long, ShopCategoryItem> bindMap = ArrayListMultimap.create();

      for(ShopCategoryItem bind : binds) {
         bindMap.put(bind.getItemId(), bind);
      }

      return bindMap;
   }
}
