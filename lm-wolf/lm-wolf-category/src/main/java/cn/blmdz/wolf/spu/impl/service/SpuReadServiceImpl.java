package cn.blmdz.wolf.spu.impl.service;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.google.common.base.Throwables;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.spu.dto.FullSpu;
import cn.blmdz.wolf.spu.impl.dao.SkuTemplateDao;
import cn.blmdz.wolf.spu.impl.dao.SpuAttributeDao;
import cn.blmdz.wolf.spu.impl.dao.SpuDao;
import cn.blmdz.wolf.spu.impl.dao.SpuDetailDao;
import cn.blmdz.wolf.spu.model.SkuTemplate;
import cn.blmdz.wolf.spu.model.Spu;
import cn.blmdz.wolf.spu.model.SpuAttribute;
import cn.blmdz.wolf.spu.model.SpuDetail;
import cn.blmdz.wolf.spu.service.SpuReadService;

@Service
public class SpuReadServiceImpl implements SpuReadService {
   private static final Logger log = LoggerFactory.getLogger(SpuReadServiceImpl.class);
   private final SpuDao spuDao;
   private final SpuDetailDao spuDetailDao;
   private final SpuAttributeDao spuAttributeDao;
   private final SkuTemplateDao skuTemplateDao;

   @Autowired
   public SpuReadServiceImpl(SpuDao spuDao, SpuDetailDao spuDetailDao, SpuAttributeDao spuAttributeDao, SkuTemplateDao skuTemplateDao) {
      this.spuDao = spuDao;
      this.spuDetailDao = spuDetailDao;
      this.spuAttributeDao = spuAttributeDao;
      this.skuTemplateDao = skuTemplateDao;
   }

   public Response findByCategoryId(Long categoryId, String keyword, Integer pageNo, Integer pageSize) {
      try {
         PageInfo pageInfo = new PageInfo(pageNo, pageSize);
         Map<String, Object> params = Maps.newHashMap();
         if(StringUtils.hasText(keyword)) {
            params.put("name", keyword);
         }

         params.put("categoryId", categoryId);
         return Response.ok(this.spuDao.paging(pageInfo.getOffset(), pageInfo.getLimit(), params));
      } catch (Exception var7) {
         log.error("failed to paging spus(categoryId={}, name={},pageNo={}, pageSize={}), cause: {}", new Object[]{categoryId, keyword, pageNo, pageSize, Throwables.getStackTraceAsString(var7)});
         return Response.fail("spu.find.fail");
      }
   }

   public Response findFullInfoBySpuId(Long spuId) {
      try {
         FullSpu fullSpu = new FullSpu();
         Spu spu = (Spu)this.spuDao.findById(spuId);
         if(spu == null) {
            log.error("spu(id={}) not found", spuId);
            return Response.fail("spu.not.found");
         } else {
            SpuDetail spuDetail = this.spuDetailDao.findBySpuId(spuId);
            if(spuDetail == null) {
               log.error("spu detail (spuId={}) not found", spuId);
               return Response.fail("spu.detail.not.found");
            } else {
               SpuAttribute spuAttribute = this.spuAttributeDao.findBySpuId(spuId);
               if(spuAttribute == null) {
                  log.error("spu attribute(spuId={}) not found", spuId);
                  return Response.fail("spu.attribute.not.found");
               } else {
                  List<SkuTemplate> skuTemplates = this.skuTemplateDao.findBySpuId(spuId);
                  fullSpu.setSpu(spu);
                  fullSpu.setSpuDetail(spuDetail);
                  fullSpu.setSkuTemplates(skuTemplates);
                  fullSpu.setGroupedSkuAttributes(spuAttribute.getSkuAttrs());
                  fullSpu.setGroupedOtherAttributes(spuAttribute.getOtherAttrs());
                  return Response.ok(fullSpu);
               }
            }
         }
      } catch (Exception var7) {
         log.error("failed to find full info for spu(id={}), cause:{}", spuId, Throwables.getStackTraceAsString(var7));
         return Response.fail("spu.full.find.fail");
      }
   }

   public Response findById(Long spuId) {
      try {
         Spu spu = (Spu)this.spuDao.findById(spuId);
         if(spu == null) {
            log.error("spu(id={}) not found", spuId);
            return Response.fail("spu.not.found");
         } else {
            return Response.ok(spu);
         }
      } catch (Exception var3) {
         log.error("failed to find spu(id={}), cause:{}", spuId, Throwables.getStackTraceAsString(var3));
         return Response.fail("spu.find.fail");
      }
   }

   public Response findRichTextById(Long spuId) {
      try {
         SpuDetail spuDetail = this.spuDetailDao.findBySpuId(spuId);
         if(spuDetail == null) {
            log.error("spu detail(spuId={}) not found", spuId);
            return Response.fail("spu.detail.not.found");
         } else {
            return Response.ok(spuDetail.getDetail());
         }
      } catch (Exception var3) {
         log.error("failed to find spu detail for spu(id={}),cause:{}", spuId, Throwables.getStackTraceAsString(var3));
         return Response.fail("spu.detail.find.fail");
      }
   }
}
