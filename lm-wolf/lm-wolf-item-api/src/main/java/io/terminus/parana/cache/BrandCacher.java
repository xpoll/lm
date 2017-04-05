package io.terminus.parana.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Splitters;
import io.terminus.pampas.client.Export;
import io.terminus.parana.brand.model.Brand;
import io.terminus.parana.brand.service.BrandReadService;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class BrandCacher {
   private static final Logger log = LoggerFactory.getLogger(BrandCacher.class);
   private final LoadingCache brandCache;

   @Autowired
   public BrandCacher(final BrandReadService brandReadService, @Value("${cache.duration: 60}") Integer duration) {
      this.brandCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader() {
         public Brand load(Long brandId) throws Exception {
            Response<Brand> rBrand = brandReadService.findById(brandId);
            if(!rBrand.isSuccess()) {
               BrandCacher.log.error("failed to find brand(id={}), error code:{}", brandId, rBrand.getError());
               throw new ServiceException("find brand fail,code: " + rBrand.getError());
            } else {
               return (Brand)rBrand.getResult();
            }
         }
      });
   }

   public Brand findBrandById(Long brandId) {
      return (Brand)this.brandCache.getUnchecked(brandId);
   }

   @Export(
      paramNames = {"brandIds"}
   )
   public List findBrandByIds(String brandIds) {
      List<String> ids = Splitters.COMMA.splitToList(brandIds);
      List<Brand> brands = Lists.newArrayListWithCapacity(ids.size());

      for(String id : ids) {
         brands.add(this.brandCache.getUnchecked(Long.valueOf(id)));
      }

      return brands;
   }
}
