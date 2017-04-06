package cn.blmdz.wolf.parana.cache;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.parana.category.dto.FrontCategoryTree;
import cn.blmdz.wolf.parana.category.model.FrontCategory;
import cn.blmdz.wolf.parana.category.service.FrontCategoryReadService;

@Component
public class FrontCategoryCacher {
   private static final Logger log = LoggerFactory.getLogger(FrontCategoryCacher.class);
   private final FrontCategoryReadService frontCategoryReadService;
   private final LoadingCache frontCategoryCache;

   @Autowired
   public FrontCategoryCacher(FrontCategoryReadService frontCategoryReadService, @Value("${cache.duration: 60}") Integer duration) {
      this.frontCategoryReadService = frontCategoryReadService;
      this.frontCategoryCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<String, List>() {
         public List load(String ids) throws Exception {
            if(StringUtils.isBlank(ids)) {
               return Collections.emptyList();
            } else {
               List<String> parts = Splitters.COMMA.splitToList(ids);
               List<Long> fcIds = Lists.newArrayListWithCapacity(parts.size());

               for(String part : parts) {
                  fcIds.add(Long.valueOf(Long.parseLong(part)));
               }

               return FrontCategoryCacher.this.buildForest(fcIds);
            }
         }
      });
   }

   private List<FrontCategoryTree> buildForest(List ids) {
      Response<List<FrontCategory>> r = this.frontCategoryReadService.findByIds(ids);
      if(!r.isSuccess()) {
         log.error("failed to find front categories(ids={}), error code:{}", ids, r.getError());
         throw new ServiceException(r.getError());
      } else {
         List<FrontCategory> frontCategories = (List)r.getResult();
         List<FrontCategoryTree> result = Lists.newArrayListWithCapacity(frontCategories.size());

         for(FrontCategory frontCategory : frontCategories) {
            result.add(this.buildTree(frontCategory));
         }

         return result;
      }
   }

   private FrontCategoryTree buildTree(FrontCategory frontCategory) {
      FrontCategoryTree frontCategoryTree = new FrontCategoryTree();
      frontCategoryTree.setCurrent(frontCategory);
      Long frontCategoryId = frontCategory.getId();
      Response<List<FrontCategory>> r = this.frontCategoryReadService.findChildrenByPid(frontCategoryId);
      if(!r.isSuccess()) {
         log.error("failed to find children of front categories(id={}), error code:{}", frontCategoryId, r.getError());
         throw new ServiceException(r.getError());
      } else {
         List<FrontCategoryTree> children = frontCategoryTree.getChildren();

         for(FrontCategory fc : r.getResult()) {
            children.add(this.buildTree(fc));
         }

         return frontCategoryTree;
      }
   }

   @Export(
      paramNames = {"ids"}
   )
   public List findByIds(String ids) {
      return (List)this.frontCategoryCache.getUnchecked(ids);
   }
}
