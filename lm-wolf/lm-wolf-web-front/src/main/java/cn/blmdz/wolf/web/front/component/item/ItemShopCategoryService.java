package cn.blmdz.wolf.web.front.component.item;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.category.dto.ShopCategoryWithChildren;
import cn.blmdz.wolf.category.model.ShopCategory;
import cn.blmdz.wolf.category.model.ShopCategoryItem;
import cn.blmdz.wolf.category.service.ShopCategoryItemReadService;
import cn.blmdz.wolf.category.service.ShopCategoryReadService;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.common.utils.Iters;
import cn.blmdz.wolf.item.dto.ItemWithShopCategory;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.service.ItemReadService;
import cn.blmdz.wolf.web.front.util.ShopCategoryUtils;

@Service
public class ItemShopCategoryService {
   private static final Logger log = LoggerFactory.getLogger(ItemShopCategoryService.class);
   private final ItemReadService itemReadService;
   private final ShopCategoryReadService shopCategoryReadService;
   private final ShopCategoryItemReadService shopCategoryItemReadService;

   @Autowired
   public ItemShopCategoryService(ItemReadService itemReadService, ShopCategoryReadService shopCategoryReadService, ShopCategoryItemReadService shopCategoryItemReadService) {
      this.itemReadService = itemReadService;
      this.shopCategoryReadService = shopCategoryReadService;
      this.shopCategoryItemReadService = shopCategoryItemReadService;
   }

   @Export(
      paramNames = {"shopCategoryId", "pageNo", "pageSize"}
   )
   public Response findByShopIdAndCategoryId(Long shopCategoryId, Integer pageNo, Integer pageSize) {
      try {
         ParanaUser user = (ParanaUser)UserUtil.getCurrentUser();
         Long shopId = user.getShopId();
         if(shopId == null) {
            log.warn("no shopId for login user={}", user);
            return Response.fail("user.no.permission");
         } else {
            if(shopCategoryId == null) {
               shopCategoryId = Long.valueOf(-1L);
            }

            Response<Paging<Long>> pagingResp = this.shopCategoryItemReadService.findByShopIdAndCategoryId(shopId, shopCategoryId, pageNo, pageSize);
            if(!pagingResp.isSuccess()) {
               log.warn("paging shopCategoryItems failed, error={}", pagingResp.getError());
               return Response.fail(pagingResp.getError());
            } else {
               Paging<Long> paging = (Paging)pagingResp.getResult();
               List<Long> itemIds = Iters.nullToEmpty(paging.getData());
               List<ItemWithShopCategory> result = Lists.newArrayList();
               if(!itemIds.isEmpty()) {
                  Map<Long, Item> itemMap = this.buildItemMap(itemIds);
                  List<ShopCategoryWithChildren> tree = this.buildTree(shopId);
                  Multimap<Long, ShopCategoryItem> bindMap = this.buildBindMap(shopId, itemIds);

                  for(Long itemId : itemIds) {
                     Item item = (Item)itemMap.get(itemId);
                     if(item != null) {
                        Collection<ShopCategoryItem> sci = bindMap.get(itemId);
                        List<ShopCategory> shopCategories = Lists.newArrayList();

                        for(ShopCategoryItem categoryItem : sci) {
                           Long categoryId = categoryItem.getShopCategoryId();
                           if(categoryId != null && categoryId.longValue() > 0L) {
                              ShopCategory sc = ShopCategoryUtils.fixName(tree, categoryId, shopId);
                              if(sc != null) {
                                 shopCategories.add(sc);
                              }
                           }
                        }

                        ItemWithShopCategory itemWithSc = new ItemWithShopCategory();
                        BeanUtils.copyProperties(item, itemWithSc);
                        itemWithSc.setShopCategories(shopCategories);
                        result.add(itemWithSc);
                     }
                  }
               }

               return Response.ok(new Paging(paging.getTotal(), result));
            }
         }
      } catch (ServiceException var22) {
         log.warn("failed to find ShopCategoryItem(shopCategoryId={}, pageNo={}, pageSize={}), error={}", new Object[]{shopCategoryId, pageNo, pageSize, var22.getMessage()});
         return Response.fail(var22.getMessage());
      } catch (Exception var23) {
         log.error("failed to find ShopCategoryItem(shopCategoryId={}, pageNo={}, pageSize={}), cause:{}", new Object[]{shopCategoryId, pageNo, pageSize, Throwables.getStackTraceAsString(var23)});
         return Response.fail("shop.category.item.find.fail");
      }
   }

   private Map buildItemMap(List itemIds) {
      Response<List<Item>> resp = this.itemReadService.findByIds(itemIds);
      if(!resp.isSuccess()) {
         log.warn("find items by ids failed, itemIds={}, error={}", itemIds, resp.getError());
         throw new ServiceException(resp.getError());
      } else {
         Map<Long, Item> itemMap = Maps.newHashMap();

         for(Item item : resp.getResult()) {
            itemMap.put(item.getId(), item);
         }

         return itemMap;
      }
   }

   private List buildTree(Long shopId) {
      Response<List<ShopCategoryWithChildren>> resp = this.shopCategoryReadService.findEntireTreeByShopId(shopId);
      if(!resp.isSuccess()) {
         log.warn("find entire tree by shopId={} failed, error={}", shopId, resp.getError());
         throw new ServiceException(resp.getError());
      } else {
         return (List)resp.getResult();
      }
   }

   private Multimap buildBindMap(Long shopId, List itemIds) {
      Response<List<ShopCategoryItem>> resp = this.shopCategoryItemReadService.findByShopIdAndItemIds(shopId, itemIds);
      if(!resp.isSuccess()) {
         log.warn("find binds failed, shopId={}, itemIds={}, error={}", new Object[]{shopId, itemIds, resp.getError()});
         throw new ServiceException(resp.getError());
      } else {
         Multimap<Long, ShopCategoryItem> map = ArrayListMultimap.create();

         for(ShopCategoryItem bind : resp.getResult()) {
            map.put(bind.getItemId(), bind);
         }

         return map;
      }
   }
}
