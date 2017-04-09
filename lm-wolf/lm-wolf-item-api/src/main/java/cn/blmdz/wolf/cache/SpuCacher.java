package cn.blmdz.wolf.cache;

import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.spu.dto.FullSpu;
import cn.blmdz.wolf.spu.service.SpuReadService;

@Component
public class SpuCacher {
   private static final Logger log = LoggerFactory.getLogger(SpuCacher.class);
   private final LoadingCache<Long, FullSpu> spuCacher;

   @Autowired
   public SpuCacher(final SpuReadService spuReadService, @Value("${cache.duration: 60}") Integer duration) {
      this.spuCacher = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<Long, FullSpu>() {
         public FullSpu load(Long spuId) throws Exception {
            Response<FullSpu> rFullSpu = spuReadService.findFullInfoBySpuId(spuId);
            if(!rFullSpu.isSuccess()) {
               SpuCacher.log.error("failed to find full spu(id={}), error code:{}", spuId, rFullSpu.getError());
               throw new ServiceException("find full spu fail,error code: " + rFullSpu.getError());
            } else {
               return (FullSpu)rFullSpu.getResult();
            }
         }
      });
   }

   public FullSpu findFullSpuById(Long spuId) {
      return (FullSpu)this.spuCacher.getUnchecked(spuId);
   }
}
