package io.terminus.parana.spu.impl.manager;

import io.terminus.parana.category.impl.dao.BackCategoryDao;
import io.terminus.parana.category.model.BackCategory;
import io.terminus.parana.spu.impl.dao.SkuTemplateDao;
import io.terminus.parana.spu.impl.dao.SpuAttributeDao;
import io.terminus.parana.spu.impl.dao.SpuDao;
import io.terminus.parana.spu.impl.dao.SpuDetailDao;
import io.terminus.parana.spu.model.SkuTemplate;
import io.terminus.parana.spu.model.Spu;
import io.terminus.parana.spu.model.SpuAttribute;
import io.terminus.parana.spu.model.SpuDetail;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class SpuManager {
   private static final Logger log = LoggerFactory.getLogger(SpuManager.class);
   private final SpuDao spuDao;
   private final SkuTemplateDao skuTemplateDao;
   private final SpuDetailDao spuDetailDao;
   private final SpuAttributeDao spuAttributeDao;
   private final BackCategoryDao backCategoryDao;

   @Autowired
   public SpuManager(SpuDao spuDao, SkuTemplateDao skuTemplateDao, SpuDetailDao spuDetailDao, SpuAttributeDao spuAttributeDao, BackCategoryDao backCategoryDao) {
      this.spuDao = spuDao;
      this.skuTemplateDao = skuTemplateDao;
      this.spuDetailDao = spuDetailDao;
      this.spuAttributeDao = spuAttributeDao;
      this.backCategoryDao = backCategoryDao;
   }

   @Transactional
   public Long createSpu(Long deletedSpuId, Spu spu, SpuDetail spuDetail, SpuAttribute spuAttribute, List skuTemplates, BackCategory backCategory) {
      if(deletedSpuId != null) {
         this.spuDao.delete(deletedSpuId);
         this.spuDetailDao.deleteBySpuId(deletedSpuId);
         this.spuAttributeDao.deleteBySpuId(deletedSpuId);
         this.skuTemplateDao.deleteBySpuId(deletedSpuId);
      }

      this.spuDao.create(spu);
      Long spuId = spu.getId();
      spuDetail.setSpuId(spuId);
      this.spuDetailDao.create(spuDetail);
      spuAttribute.setSpuId(spuId);
      this.spuAttributeDao.create(spuAttribute);
      this.persistSkuTemplates(spu, skuTemplates);
      this.backCategoryDao.update(backCategory);
      return spuId;
   }

   private void persistSkuTemplates(Spu spu, List skuTemplates) {
      for(SkuTemplate skuTemplate : skuTemplates) {
         skuTemplate.setSpuId(spu.getId());
         skuTemplate.setStockType(spu.getStockType());
         skuTemplate.setStatus(spu.getStatus());
         this.skuTemplateDao.create(skuTemplate);
      }

   }

   @Transactional
   public void updateSpu(Spu spu, SpuDetail spuDetail, SpuAttribute spuAttribute, List skuTemplates) {
      this.spuDao.update(spu);
      Long spuId = spu.getId();
      spuDetail.setSpuId(spuId);
      this.spuDetailDao.update(spuDetail);
      spuAttribute.setSpuId(spuId);
      this.spuAttributeDao.update(spuAttribute);
      this.skuTemplateDao.updateStatusBySpuId(spuId, Integer.valueOf(-3));
      this.createOrUpdateSkuTemplates(spu, skuTemplates);
   }

   private void createOrUpdateSkuTemplates(Spu spu, List skuTemplates) {
      for(SkuTemplate skuTemplate : skuTemplates) {
         skuTemplate.setSpuId(spu.getId());
         skuTemplate.setStatus(spu.getStatus());
         skuTemplate.setStockType(spu.getStockType());
         if(skuTemplate.getId() != null) {
            this.skuTemplateDao.update(skuTemplate);
         } else {
            this.skuTemplateDao.create(skuTemplate);
         }
      }

   }

   @Transactional
   public Spu delete(Spu spu) {
      Long spuId = spu.getId();
      Long categoryId = spu.getCategoryId();
      Long spuCount = this.spuDao.countOfValidSpu(categoryId);
      if(spuCount.longValue() == 1L) {
         BackCategory backCategory = new BackCategory();
         backCategory.setId(categoryId);
         backCategory.setHasSpu(Boolean.valueOf(false));
         this.backCategoryDao.update(backCategory);
      }

      this.spuDao.updateStatus(spuId, Integer.valueOf(-1));
      this.skuTemplateDao.updateStatusBySpuId(spuId, Integer.valueOf(-3));
      return spu;
   }

   @Transactional
   public void updateRichText(String spuInfoMd5, SpuDetail spuDetail) {
      this.spuDao.updateSpuInfoMd5(spuDetail.getSpuId(), spuInfoMd5);
      this.spuDetailDao.update(spuDetail);
   }
}
