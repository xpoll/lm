package io.terminus.parana.cache;

import com.google.common.base.MoreObjects;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Joiners;
import io.terminus.common.utils.Splitters;
import io.terminus.pampas.client.Export;
import io.terminus.parana.category.dto.ShopCategoryWithChildren;
import io.terminus.parana.category.model.ShopCategory;
import io.terminus.parana.category.service.ShopCategoryReadService;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ShopCategoryCacher {
   private static final Logger log = LoggerFactory.getLogger(ShopCategoryCacher.class);
   private final LoadingCache shopCategoryCache;
   private final LoadingCache shopCategoryAllCache;

   @Autowired
   public ShopCategoryCacher(final ShopCategoryReadService shopCategoryReadService, @Value("${cache.duration: 60}") Integer duration) {
      this.shopCategoryCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader() {
         public List load(String key) throws Exception {
            List<String> shopIdAndCategoryId = Splitters.COLON.splitToList(key);
            Long shopId = Long.valueOf(Long.parseLong((String)shopIdAndCategoryId.get(0)));
            Long categoryId = Long.valueOf(Long.parseLong((String)shopIdAndCategoryId.get(1)));
            Response<List<ShopCategory>> rShopCategory;
            if(categoryId.longValue() > 0L) {
               rShopCategory = shopCategoryReadService.findChildrenByShopIdAndPid(shopId, categoryId);
            } else {
               rShopCategory = shopCategoryReadService.findChildrenByShopId(shopId);
            }

            if(!rShopCategory.isSuccess()) {
               ShopCategoryCacher.log.error("failed to find children category of shop(shopId={},pid={}), error code:{}", new Object[]{shopId, categoryId, rShopCategory.getError()});
               throw new ServiceException("find shop category fail,code: " + rShopCategory.getError());
            } else {
               return (List)rShopCategory.getResult();
            }
         }
      });
      this.shopCategoryAllCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader() {
         public List load(Long key) throws Exception {
            Response<List<ShopCategoryWithChildren>> resp = shopCategoryReadService.findEntireTreeByShopId(key);
            if(!resp.isSuccess()) {
               ShopCategoryCacher.log.error("find all shop category by shopId={} failed, error={}", key, resp.getError());
               throw new ServiceException(resp.getError());
            } else {
               return (List)resp.getResult();
            }
         }
      });
   }

   @Export(
      paramNames = {"shopId", "categoryId"}
   )
   public List findChildrenOf(Long shopId, Long categoryId) {
      Long shopCategoryId = (Long)MoreObjects.firstNonNull(categoryId, Long.valueOf(0L));
      return (List)this.shopCategoryCache.getUnchecked(Joiners.COLON.join(shopId, shopCategoryId, new Object[0]));
   }

   @Export(
      paramNames = {"shopId"}
   )
   public List findEntireTreeOf(Long shopId) {
      return shopId == null?Collections.emptyList():(List)this.shopCategoryAllCache.getUnchecked(shopId);
   }
}
