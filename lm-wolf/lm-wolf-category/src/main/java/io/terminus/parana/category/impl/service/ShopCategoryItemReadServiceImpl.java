package io.terminus.parana.category.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.parana.category.impl.dao.ShopCategoryItemDao;
import io.terminus.parana.category.model.ShopCategoryItem;
import io.terminus.parana.category.service.ShopCategoryItemReadService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ShopCategoryItemReadServiceImpl implements ShopCategoryItemReadService {
   private static final Logger log = LoggerFactory.getLogger(ShopCategoryItemReadServiceImpl.class);
   private final ShopCategoryItemDao shopCategoryItemDao;

   @Autowired
   public ShopCategoryItemReadServiceImpl(ShopCategoryItemDao shopCategoryItemDao) {
      this.shopCategoryItemDao = shopCategoryItemDao;
   }

   public Response findByShopIdAndItemId(Long shopId, Long itemId) {
      try {
         List<ShopCategoryItem> shopCategoryItems = this.shopCategoryItemDao.findByShopIdAndItemId(shopId, itemId);
         return Response.ok(shopCategoryItems);
      } catch (Exception var4) {
         log.error("failed to find ShopCategoryItem(shopId={}, itemId={}), cause:{}", new Object[]{shopId, itemId, Throwables.getStackTraceAsString(var4)});
         return Response.fail("shop.category.item.find.fail");
      }
   }

   public Response findByShopIdAndItemIds(Long shopId, List itemIds) {
      try {
         List<ShopCategoryItem> shopCategoryItems = this.shopCategoryItemDao.findByShopIdAndItemIds(shopId, itemIds);
         return Response.ok(shopCategoryItems);
      } catch (Exception var4) {
         log.error("failed to find ShopCategoryItem(shopId={}, itemIds={}), cause:{}", new Object[]{shopId, itemIds, Throwables.getStackTraceAsString(var4)});
         return Response.fail("shop.category.item.find.fail");
      }
   }

   public Response findByShopIdAndCategoryId(Long shopId, Long shopCategoryId, Integer pageNo, Integer pageSize) {
      try {
         PageInfo pageInfo = new PageInfo(pageNo, pageSize);
         Paging<Long> items;
         if(shopCategoryId != null && shopCategoryId.longValue() != -1L) {
            items = this.shopCategoryItemDao.findByShopIdAndCategoryId(shopId, shopCategoryId, pageInfo.getOffset(), pageInfo.getLimit());
         } else {
            items = this.shopCategoryItemDao.findUnknownByShopId(shopId, pageInfo.getOffset(), pageInfo.getLimit());
         }

         return Response.ok(items);
      } catch (Exception var7) {
         log.error("failed to find ShopCategoryItem(shopId={}, shopCategoryId={}), cause:{}", new Object[]{shopId, shopCategoryId, Throwables.getStackTraceAsString(var7)});
         return Response.fail("shop.category.item.find.fail");
      }
   }
}
