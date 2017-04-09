package cn.blmdz.wolf.web.admin.spu;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.spu.model.Spu;
import cn.blmdz.wolf.spu.service.SpuReadService;
import cn.blmdz.wolf.spu.service.SpuWriteService;
import cn.blmdz.wolf.web.core.util.RichTextCleaner;

@RestController
@RequestMapping({"/api/spu/{id}/detail"})
public class Richtexts {
   private static final Logger log = LoggerFactory.getLogger(Richtexts.class);
   private final SpuReadService spuReadService;
   private final SpuWriteService spuWriteService;

   @Autowired
   public Richtexts(SpuReadService spuReadService, SpuWriteService spuWriteService) {
      this.spuReadService = spuReadService;
      this.spuWriteService = spuWriteService;
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Boolean editRichText(@PathVariable("id") Long spuId, @RequestParam("detail") String richText) {
      Response<Spu> rSpu = this.spuReadService.findById(spuId);
      if(!rSpu.isSuccess()) {
         log.error("failed to find spu(id={}), error code:{}", spuId, rSpu.getError());
         throw new JsonResponseException(rSpu.getError());
      } else {
         Spu spu = (Spu)rSpu.getResult();
         String safeRichText = RichTextCleaner.safe(richText);
         Response<Boolean> r = this.spuWriteService.editRichText(spuId, safeRichText);
         if(!r.isSuccess()) {
            log.error("failed to edit richtext for spu(id={}), error code:{}", spuId, r.getError());
            throw new JsonResponseException(r.getError());
         } else {
            return Boolean.valueOf(true);
         }
      }
   }

   @RequestMapping(
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public String findRichTextById(@PathVariable("id") Long spuId) {
      Response<String> r = this.spuReadService.findRichTextById(spuId);
      if(!r.isSuccess()) {
         log.error("failed to find rich text detail for spu(id={}), error code:{}", spuId, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (String)r.getResult();
      }
   }
}
