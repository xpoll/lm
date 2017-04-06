package cn.blmdz.wolf.spu.impl.service;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.impl.dao.BackCategoryDao;
import cn.blmdz.wolf.parana.category.model.BackCategory;
import cn.blmdz.wolf.parana.item.common.Digestors;
import cn.blmdz.wolf.parana.spu.dto.FullSpu;
import cn.blmdz.wolf.parana.spu.model.Spu;
import cn.blmdz.wolf.parana.spu.model.SpuAttribute;
import cn.blmdz.wolf.parana.spu.model.SpuDetail;
import cn.blmdz.wolf.parana.spu.service.SpuWriteService;
import cn.blmdz.wolf.spu.impl.dao.SpuAttributeDao;
import cn.blmdz.wolf.spu.impl.dao.SpuDao;
import cn.blmdz.wolf.spu.impl.dao.SpuDetailDao;
import cn.blmdz.wolf.spu.impl.manager.SpuManager;

@Service
public class SpuWriteServiceImpl implements SpuWriteService {
   private static final Logger log = LoggerFactory.getLogger(SpuWriteServiceImpl.class);
   private final SpuManager spuManager;
   private final SpuDao spuDao;
   private final SpuDetailDao spuDetailDao;
   private final SpuAttributeDao spuAttributeDao;
   private final BackCategoryDao backCategoryDao;

   @Autowired
   public SpuWriteServiceImpl(SpuManager spuManager, SpuDao spuDao, SpuDetailDao spuDetailDao, SpuAttributeDao spuAttributeDao, BackCategoryDao backCategoryDao) {
      this.spuManager = spuManager;
      this.spuDao = spuDao;
      this.spuDetailDao = spuDetailDao;
      this.spuAttributeDao = spuAttributeDao;
      this.backCategoryDao = backCategoryDao;
   }

   public Response create(FullSpu fullSpu) {
      try {
         Spu spu = fullSpu.getSpu();
         spu.setStatus((Integer)MoreObjects.firstNonNull(spu.getStatus(), Integer.valueOf(1)));
         Long categoryId = spu.getCategoryId();
         BackCategory backCategory = (BackCategory)this.backCategoryDao.findById(categoryId);
         if(backCategory == null) {
            log.error("back category(id={}) not found", categoryId);
            return Response.fail("category.not.found");
         } else if(backCategory.getHasChildren().booleanValue()) {
            log.error("back category(id={}) is not leaf", categoryId);
            return Response.fail("category.not.leaf");
         } else {
            Spu nameExists = this.spuDao.findByCategoryIdAndName(categoryId, spu.getName());
            if(nameExists != null && Objects.equal(nameExists.getStatus(), Integer.valueOf(1))) {
               log.error("duplicate spu name({}) of category(id={})", spu.getName(), categoryId);
               return Response.fail("spu.name.duplicated");
            } else {
               Long deletedSpuId = nameExists != null?nameExists.getId():null;
               backCategory.setHasSpu(Boolean.valueOf(true));
               SpuAttribute spuAttribute = new SpuAttribute();
               spuAttribute.setOtherAttrs(fullSpu.getGroupedOtherAttributes());
               spuAttribute.setSkuAttrs(fullSpu.getGroupedSkuAttributes());
               String spuInfoMd5 = Digestors.spuDigest(spu, fullSpu.getSpuDetail(), spuAttribute);
               spu.setSpuInfoMd5(spuInfoMd5);
               Long spuId = this.spuManager.createSpu(deletedSpuId, spu, fullSpu.getSpuDetail(), spuAttribute, fullSpu.getSkuTemplates(), backCategory);
               return Response.ok(spuId);
            }
         }
      } catch (Exception var10) {
         log.error("failed to create {}, cause:{}", fullSpu, Throwables.getStackTraceAsString(var10));
         return Response.fail("spu.create.fail");
      }
   }

   public Response update(FullSpu fullSpu) {
      try {
         Spu spu = fullSpu.getSpu();
         if(spu.getStatus() == null) {
            Spu spuInDB = (Spu)this.spuDao.findById(spu.getId());
            if(spuInDB == null) {
               log.error("spu(id={}) not found", spu.getId());
               return Response.fail("spu.not.found");
            }

            spu.setStatus(spuInDB.getStatus());
         }

         SpuAttribute spuAttribute = new SpuAttribute();
         spuAttribute.setOtherAttrs(fullSpu.getGroupedOtherAttributes());
         spuAttribute.setSkuAttrs(fullSpu.getGroupedSkuAttributes());
         String richText = this.spuDetailDao.findBySpuId(spu.getId()).getDetail();
         SpuDetail spuDetail = fullSpu.getSpuDetail();
         if(spuDetail == null) {
            spuDetail = new SpuDetail();
         }

         spuDetail.setSpuId(spu.getId());
         spuDetail.setDetail(richText);
         String spuInfoMd5 = Digestors.spuDigest(spu, spuDetail, spuAttribute);
         spu.setSpuInfoMd5(spuInfoMd5);
         this.spuManager.updateSpu(spu, spuDetail, spuAttribute, fullSpu.getSkuTemplates());
         return Response.ok(Boolean.TRUE);
      } catch (Exception var7) {
         log.error("failed to update {}, cause:{}", fullSpu, Throwables.getStackTraceAsString(var7));
         return Response.fail("spu.update.fail");
      }
   }

   public Response delete(Long spuId) {
      try {
         Spu spu = (Spu)this.spuDao.findById(spuId);
         if(spu == null) {
            log.error("spu(id={}) not found", spuId);
            return Response.fail("spu.not.found");
         } else {
            this.spuManager.delete(spu);
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var3) {
         log.error("failed to delete spu(id={}), cause:{}", spuId, Throwables.getStackTraceAsString(var3));
         return Response.fail("spu.delete.fail");
      }
   }

   public Response editRichText(Long spuId, String richText) {
      try {
         SpuDetail spuDetail = this.spuDetailDao.findBySpuId(spuId);
         spuDetail.setDetail(richText);
         Spu spu = (Spu)this.spuDao.findById(spuId);
         SpuAttribute spuAttribute = this.spuAttributeDao.findBySpuId(spuId);
         String spuInfoMd5 = Digestors.spuDigest(spu, spuDetail, spuAttribute);
         spu.setSpuInfoMd5(spuInfoMd5);
         this.spuManager.updateRichText(spuInfoMd5, spuDetail);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var7) {
         log.error("failed to edit rich text for spu(id={}), cause:{}", spuId, Throwables.getStackTraceAsString(var7));
         return Response.fail("spu.detail.update.fail");
      }
   }

   public Response extras(Long spuId, Map extras) {
      try {
         SpuDetail spuDetail = this.spuDetailDao.findBySpuId(spuId);
         Spu spu = (Spu)this.spuDao.findById(spuId);
         spu.setExtra(extras);
         SpuAttribute spuAttribute = this.spuAttributeDao.findBySpuId(spuId);
         String spuInfoMd5 = Digestors.spuDigest(spu, spuDetail, spuAttribute);
         this.spuDao.updateExtras(spuId, extras, spuInfoMd5);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var7) {
         log.error("failed to update spu(id={}) extras to {}, cause:{}", new Object[]{spuId, extras, Throwables.getStackTraceAsString(var7)});
         return Response.fail("spu.update.fail");
      }
   }
}
