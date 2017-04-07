package cn.blmdz.wolf.parana.cache;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.base.MoreObjects;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Joiners;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.parana.category.dto.ShopCategoryWithChildren;
import cn.blmdz.wolf.parana.category.model.ShopCategory;
import cn.blmdz.wolf.parana.category.service.ShopCategoryReadService;

@Component
public class ShopCategoryCacher {
	private static final Logger log = LoggerFactory.getLogger(ShopCategoryCacher.class);
	private final LoadingCache<String, List<ShopCategory>> shopCategoryCache;
	private final LoadingCache<Long, List<ShopCategoryWithChildren>> shopCategoryAllCache;
	private final LoadingCache<Long, ShopCategory> shopCategoryIdCache;

	   @Autowired
	   public ShopCategoryCacher(final ShopCategoryReadService shopCategoryReadService, @Value("${cache.duration: 60}") Integer duration) {
	      this.shopCategoryCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<String, List<ShopCategory>>() {
	         public List<ShopCategory> load(String key) throws Exception {
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
	      this.shopCategoryAllCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<Long, List<ShopCategoryWithChildren>>() {
	         public List<ShopCategoryWithChildren> load(Long key) throws Exception {
	            Response<List<ShopCategoryWithChildren>> resp = shopCategoryReadService.findEntireTreeByShopId(key);
	            if(!resp.isSuccess()) {
	               ShopCategoryCacher.log.error("find all shop category by shopId={} failed, error={}", key, resp.getError());
	               throw new ServiceException(resp.getError());
	            } else {
	               return (List)resp.getResult();
	            }
	         }
	      });
			this.shopCategoryIdCache = CacheBuilder.newBuilder().expireAfterWrite(duration.intValue(), TimeUnit.MINUTES)
					.build(new CacheLoader<Long, ShopCategory>() {
						public ShopCategory load(Long id) throws Exception {
							Response shopCategoryResp = shopCategoryReadService.findById(id);
							if (!shopCategoryResp.isSuccess()) {
								ShopCategoryCacher.log.error("fail to find shop category by id:{},cause:{}", id,
										shopCategoryResp.getError());
								throw new ServiceException(shopCategoryResp.getError());
							}
							return (ShopCategory) shopCategoryResp.getResult();
						}
					});
	   }

	@Export(paramNames = { "shopId", "categoryId" })
	public List<ShopCategory> findChildrenOf(Long shopId, Long categoryId) {
		Long shopCategoryId = (Long) MoreObjects.firstNonNull(categoryId, Long.valueOf(0L));
		return (List) this.shopCategoryCache.getUnchecked(Joiners.COLON.join(shopId, shopCategoryId, new Object[0]));
	}

	@Export(paramNames = { "shopId" })
	public List<ShopCategoryWithChildren> findEntireTreeOf(Long shopId) {
		if (shopId == null) {
			return Collections.emptyList();
		}
		return (List) this.shopCategoryAllCache.getUnchecked(shopId);
	}

	public ShopCategory findById(Long id) {
		return (ShopCategory) this.shopCategoryIdCache.getUnchecked(id);
	}
}