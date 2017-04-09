package cn.blmdz.wolf.item.impl.service;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.service.AdminItemReadService;
import cn.blmdz.wolf.shop.impl.dao.ShopDao;
import cn.blmdz.wolf.shop.model.Shop;

@Service
public class AdminItemReadServiceImpl implements AdminItemReadService {
   private static final Logger log = LoggerFactory.getLogger(AdminItemReadServiceImpl.class);
   private final ItemDao itemDao;
   private final ShopDao shopDao;

   @Autowired
   public AdminItemReadServiceImpl(ItemDao itemDao, ShopDao shopDao) {
      this.itemDao = itemDao;
      this.shopDao = shopDao;
   }

   public Response findByShopId(Long shopId, Integer pageNo, Integer pageSize) {
      try {
         PageInfo page = new PageInfo(pageNo, pageSize);
         Map<String, Object> criteria = Maps.newHashMap();
         criteria.put("shopId", shopId);
         criteria.put("offset", page.getOffset());
         criteria.put("limit", page.getLimit());
         return Response.ok(this.itemDao.paging(criteria));
      } catch (Exception var6) {
         log.error("failed to find items by (shopId={}), pageNo={}, pageSize={}, status={}, cause: {}", new Object[]{shopId, pageSize, Throwables.getStackTraceAsString(var6)});
         return Response.fail("item.find.fail");
      }
   }

   public Response findByUserId(Long userId, Integer pageNo, Integer pageSize) {
      Shop shop = null;

      try {
         shop = this.shopDao.findByUserId(userId);
         if(shop == null) {
            log.error("shop(userId={}) not found", userId);
            return Response.ok(Paging.empty());
         }
      } catch (Exception var6) {
         log.error("failed to find shop for user(id={}), cause:{}", userId, Throwables.getStackTraceAsString(var6));
         return Response.fail("item.find.fail");
      }

      return this.findByShopId(shop.getId(), pageNo, pageSize);
   }

   public Response findBy(Long itemId, Long userId, Long shopId, String itemName, Integer pageNo, Integer pageSize) {
      try {
         if(itemId != null) {
            Item item = (Item)this.itemDao.findById(itemId);
            return Response.ok(new Paging(Long.valueOf(1L), Lists.newArrayList(new Item[]{item})));
         } else {
            if(userId != null) {
               Shop shop = this.shopDao.findByUserId(userId);
               if(shop == null) {
                  return Response.ok(Paging.empty());
               }

               shopId = shop.getId();
            }

            Map<String, Object> params = Maps.newHashMap();
            params.put("shopId", shopId);
            params.put("name", itemName);
            params.put("sortBy", "id");
            params.put("sortType", Integer.valueOf(2));
            PageInfo pageInfo = new PageInfo(pageNo, pageSize);
            Paging<Item> items = this.itemDao.paging(pageInfo.getOffset(), pageInfo.getLimit(), params);
            return Response.ok(items);
         }
      } catch (Exception var10) {
         log.error("failed to find items by (itemId={}, userId={}, shopId={}, itemName={}, pageNo={}, pageSize={}), cause:{}", new Object[]{itemId, userId, shopId, itemName, pageNo, pageSize, Throwables.getStackTraceAsString(var10)});
         return Response.fail("item.find.fail");
      }
   }
}
