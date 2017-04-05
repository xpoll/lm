package io.terminus.parana.web.front.category;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.category.dto.ShopCategoryWithChildren;
import io.terminus.parana.category.model.ShopCategory;
import io.terminus.parana.category.service.ShopCategoryItemWriteService;
import io.terminus.parana.category.service.ShopCategoryReadService;
import io.terminus.parana.category.service.ShopCategoryWriteService;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.common.utils.RespHelper;
import io.terminus.parana.shop.service.ShopReadService;
import io.terminus.parana.web.front.util.ShopCategoryUtils;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api/seller/shopCategories"})
public class ShopCategories {
   private static final Logger log = LoggerFactory.getLogger(ShopCategories.class);
   private final ShopReadService shopReadService;
   private final ShopCategoryReadService shopCategoryReadService;
   private final ShopCategoryWriteService shopCategoryWriteService;
   private final ShopCategoryItemWriteService shopCategoryItemWriteService;

   @Autowired
   public ShopCategories(ShopReadService shopReadService, ShopCategoryReadService shopCategoryReadService, ShopCategoryWriteService shopCategoryWriteService, ShopCategoryItemWriteService shopCategoryItemWriteService) {
      this.shopReadService = shopReadService;
      this.shopCategoryReadService = shopCategoryReadService;
      this.shopCategoryWriteService = shopCategoryWriteService;
      this.shopCategoryItemWriteService = shopCategoryItemWriteService;
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public ShopCategory create(@RequestBody ShopCategory shopCategory) {
      Long shopId = this.getCurrentShopId();
      shopCategory.setShopId(shopId);
      Response<ShopCategory> r = this.shopCategoryWriteService.create(shopCategory);
      if(!r.isSuccess()) {
         log.warn("failed to create {}, error code:{}", shopCategory, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (ShopCategory)r.getResult();
      }
   }

   @RequestMapping(
      value = {"/{id}/name"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public boolean update(@PathVariable("id") long id, @RequestParam("name") String name) {
      Long shopId = this.getCurrentShopId();
      Response<Boolean> r = this.shopCategoryWriteService.updateName(Long.valueOf(id), shopId, name);
      if(!r.isSuccess()) {
         log.warn("failed to update shop category(id={}) name to {} ,error code:{}", new Object[]{Long.valueOf(id), name, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }

   private Long getCurrentShopId() {
      ParanaUser user = (ParanaUser)UserUtil.getCurrentUser();
      if(user == null) {
         throw new JsonResponseException(401, "user.not.login");
      } else {
         Long shopId = user.getShopId();
         if(shopId == null) {
            log.warn("no shop found for user(id={})", user.getId());
            throw new JsonResponseException(403, "user.no.permission");
         } else {
            return shopId;
         }
      }
   }

   @RequestMapping(
      value = {"/{id}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   public boolean delete(@PathVariable("id") Long id) {
      Long shopId = this.getCurrentShopId();
      Response<Boolean> r = this.shopCategoryWriteService.delete(id, shopId);
      if(!r.isSuccess()) {
         log.warn("failed to delete shop category(id={}) of shop(id={}) ,error code:{}", new Object[]{id, shopId, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }

   @RequestMapping(
      value = {"/{id}/children"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findChildrenById(@PathVariable Long id) {
      Long shopId = this.getCurrentShopId();
      Response<List<ShopCategory>> resp = this.shopCategoryReadService.findChildrenByShopIdAndPid(shopId, id);
      if(!resp.isSuccess()) {
         log.warn("find children by pid failed, pid={}, shopId={}, error={]", new Object[]{id, shopId, resp.getError()});
         throw new JsonResponseException(500, resp.getError());
      } else {
         return (List)resp.getResult();
      }
   }

   @RequestMapping(
      value = {"/tree"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findEntireTree() {
      Long shopId = this.getCurrentShopId();
      return this.findEntireTree(shopId);
   }

   private List findEntireTree(Long shopId) {
      Response<List<ShopCategoryWithChildren>> findTreeResp = this.shopCategoryReadService.findEntireTreeByShopId(shopId);
      if(!findTreeResp.isSuccess()) {
         log.warn("find entire tree by shopId={} failed, error={}", shopId, findTreeResp.getError());
         throw new JsonResponseException(500, findTreeResp.getError());
      } else {
         return (List)findTreeResp.getResult();
      }
   }

   @RequestMapping(
      value = {"/{shopCategoryId}/bind"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public List bind(@PathVariable Long shopCategoryId, @RequestParam("itemIds") Long[] itemIdArray) {
      if(itemIdArray != null && itemIdArray.length != 0) {
         Long shopId = this.getCurrentShopId();
         List<Long> itemIds = Lists.newArrayList(Sets.newHashSet(itemIdArray));
         List<ShopCategoryWithChildren> tree = this.findEntireTree(shopId);
         List<Long> shopCategoryIds = Lists.newArrayList();
         ShopCategoryUtils.findShopCategoryAncestors(tree, shopCategoryId, shopCategoryIds);
         if(shopCategoryIds.isEmpty()) {
            return Collections.emptyList();
         } else {
            Response<Boolean> resp = this.shopCategoryItemWriteService.batchCreate(shopId, shopCategoryIds, itemIds);
            if(!resp.isSuccess()) {
               log.error("batch create shopCategoryItems failed, shopId={}, shopCategoryId={}, shopCategoryIds={}, itemIds={}, error={}", new Object[]{shopId, shopCategoryId, shopCategoryIds, itemIds, resp.getError()});
               throw new JsonResponseException(500, resp.getError());
            } else {
               return this.buildShopCategoryWithFullName(shopCategoryIds, shopId, tree);
            }
         }
      } else {
         return Collections.emptyList();
      }
   }

   @RequestMapping(
      value = {"/{shopCategoryId}/unbind"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public List unbind(@PathVariable Long shopCategoryId, @RequestParam("itemIds") Long[] itemIdArray) {
      if(itemIdArray != null && itemIdArray.length != 0) {
         Long shopId = this.getCurrentShopId();
         List<Long> itemIds = Lists.newArrayList(Sets.newHashSet(itemIdArray));
         List<ShopCategoryWithChildren> tree = this.findEntireTree(shopId);
         List<Long> shopCategoryIds = Lists.newArrayList();
         ShopCategoryUtils.findShopCategoryDescendants(tree, false, shopCategoryId, shopCategoryIds);
         if(shopCategoryIds.isEmpty()) {
            return Collections.emptyList();
         } else {
            Response<Boolean> resp = this.shopCategoryItemWriteService.batchDelete(shopId, shopCategoryIds, itemIds);
            if(!resp.isSuccess()) {
               log.error("batch delete shopCategoryItems failed, shopId={}, shopCategoryId={}, shopCategoryIds={}, itemIds={}, error={}", new Object[]{shopId, shopCategoryId, shopCategoryIds, itemIds, resp.getError()});
               throw new JsonResponseException(500, resp.getError());
            } else {
               return this.buildShopCategoryWithFullName(shopCategoryIds, shopId, tree);
            }
         }
      } else {
         return Collections.emptyList();
      }
   }

   private List buildShopCategoryWithFullName(List shopCategoryIds, Long shopId, List tree) {
      List<ShopCategory> rs = Lists.newArrayList();

      for(Long shopCategoryId : shopCategoryIds) {
         ShopCategory sc = ShopCategoryUtils.fixName(tree, shopCategoryId, shopId);
         if(sc != null) {
            rs.add(sc);
         }
      }

      return rs;
   }

   @RequestMapping(
      value = {"/{shopCategoryId}/move-up"},
      method = {RequestMethod.PUT}
   )
   public Boolean moveUp(@PathVariable Long shopCategoryId) {
      Long shopId = this.getCurrentShopId();
      return (Boolean)RespHelper.or500(this.shopCategoryWriteService.move(shopCategoryId, -1));
   }

   @RequestMapping(
      value = {"/{shopCategoryId}/move-down"},
      method = {RequestMethod.PUT}
   )
   public Boolean moveDown(@PathVariable Long shopCategoryId) {
      Long shopId = this.getCurrentShopId();
      return (Boolean)RespHelper.or500(this.shopCategoryWriteService.move(shopCategoryId, 1));
   }

   @RequestMapping(
      value = {"/{shopCategoryId}/disclosed"},
      method = {RequestMethod.PUT}
   )
   public Boolean changeDisclosed(@PathVariable Long shopCategoryId, @RequestParam("value") Boolean disclosed) {
      Long shopId = this.getCurrentShopId();
      return (Boolean)RespHelper.or500(this.shopCategoryWriteService.updateDisclosed(shopCategoryId, disclosed));
   }
}
