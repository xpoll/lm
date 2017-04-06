package cn.blmdz.wolf.web.admin.spu;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.base.Splitter;
import com.google.common.eventbus.EventBus;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.component.spu.component.SpuWriter;
import cn.blmdz.wolf.parana.spu.dto.FullSpu;
import cn.blmdz.wolf.parana.spu.model.SkuTemplate;
import cn.blmdz.wolf.parana.spu.model.Spu;
import cn.blmdz.wolf.parana.spu.service.SpuReadService;
import cn.blmdz.wolf.parana.spu.service.SpuWriteService;
import cn.blmdz.wolf.web.core.events.spu.SpuCreatedEvent;
import cn.blmdz.wolf.web.core.events.spu.SpuDeletedEvent;
import cn.blmdz.wolf.web.core.events.spu.SpuUpdatedEvent;

@RestController
@RequestMapping({"/api/spu"})
public class Spus {
   private static final Logger log = LoggerFactory.getLogger(Spus.class);
   private final SpuReadService spuReadService;
   private final SpuWriteService spuWriteService;
   private final SpuWriter spuWriter;
   private final EventBus eventBus;

   @Autowired
   public Spus(SpuReadService spuReadService, SpuWriteService spuWriteService, SpuWriter spuWriter, EventBus eventBus) {
      this.spuReadService = spuReadService;
      this.spuWriteService = spuWriteService;
      this.spuWriter = spuWriter;
      this.eventBus = eventBus;
   }

   @RequestMapping(
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public Spu findById(@RequestParam(
   name = "id"
) Long id) {
      Response<Spu> rSpu = this.spuReadService.findById(id);
      if(!rSpu.isSuccess()) {
         log.error("failed to find spu(id={}),error code:{}", id, rSpu.getError());
         throw new JsonResponseException(rSpu.getError());
      } else {
         return (Spu)rSpu.getResult();
      }
   }

   @RequestMapping(
      value = {"/bycat"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public Paging findByCategoryId(@RequestParam(
   name = "categoryId"
) Long categoryId, @RequestParam(
   name = "keyword",
   required = false
) String keyword, @RequestParam(
   name = "pageNo",
   defaultValue = "1"
) Integer pageNo, @RequestParam(
   name = "pageSize",
   defaultValue = "10"
) Integer pageSize) {
      Response<Paging<Spu>> rSpu = this.spuReadService.findByCategoryId(categoryId, keyword, pageNo, pageSize);
      if(!rSpu.isSuccess()) {
         log.error("failed to find spu by category(id={}),error code:{}", categoryId, rSpu.getError());
         throw new JsonResponseException(rSpu.getError());
      } else {
         return (Paging)rSpu.getResult();
      }
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Long create(@RequestBody FullSpu fullSpu) {
      Spu spu = fullSpu.getSpu();
      this.defaultSpuInfos(spu);

      try {
         this.extractInfoFromSkus(spu, fullSpu.getSkuTemplates());
      } catch (Exception var4) {
         log.error("bad sku info", var4);
         throw new JsonResponseException("illegal.sku.info");
      }

      Response<Long> rSpuId = this.spuWriter.create(fullSpu);
      if(!rSpuId.isSuccess()) {
         log.error("failed to create {}, error code:{}", fullSpu, rSpuId.getError());
         throw new JsonResponseException(rSpuId.getError());
      } else {
         this.eventBus.post(new SpuCreatedEvent((Long)rSpuId.getResult()));
         return (Long)rSpuId.getResult();
      }
   }

   private void defaultSpuInfos(Spu spu) {
      spu.setStatus((Integer)MoreObjects.firstNonNull(spu.getStatus(), Integer.valueOf(1)));
      spu.setType((Integer)MoreObjects.firstNonNull(spu.getType(), Integer.valueOf(1)));
      spu.setReduceStockType((Integer)MoreObjects.firstNonNull(spu.getReduceStockType(), Integer.valueOf(1)));
      spu.setStockType((Integer)MoreObjects.firstNonNull(spu.getStockType(), Integer.valueOf(0)));
   }

   private void extractInfoFromSkus(Spu spu, List<SkuTemplate> skuTemplates) {
      if(Objects.equal(spu.getStockType(), Integer.valueOf(0))) {
         int stockQuantity = 0;

         for(SkuTemplate skuTemplate : skuTemplates) {
            if(skuTemplate.getStockQuantity() != null) {
               if(skuTemplate.getStockQuantity().intValue() < 0) {
                  throw new IllegalArgumentException("sku.stock.negative");
               }

               stockQuantity += skuTemplate.getStockQuantity().intValue();
            }
         }

         spu.setStockQuantity(Integer.valueOf(stockQuantity));
      }

      int highPrice = -1;
      int lowPrice = -1;

      for(SkuTemplate skuTemplate : skuTemplates) {
         if(skuTemplate.getPrice() != null) {
            if(skuTemplate.getPrice().intValue() <= 0) {
               throw new IllegalArgumentException("sku.price.need.positive");
            }

            if(skuTemplate.getPrice().intValue() > highPrice) {
               highPrice = skuTemplate.getPrice().intValue();
            }

            if(skuTemplate.getPrice().intValue() < lowPrice || lowPrice < 0) {
               lowPrice = skuTemplate.getPrice().intValue();
            }
         }
      }

      if(highPrice > 0) {
         spu.setHighPrice(Integer.valueOf(highPrice));
      }

      if(lowPrice > 0) {
         spu.setLowPrice(Integer.valueOf(lowPrice));
      }

   }

   @RequestMapping(
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public Boolean update(@RequestBody FullSpu fullSpu) {
      Spu spu = fullSpu.getSpu();
      Long spuId = spu.getId();
      Response<Spu> rSpu = this.spuReadService.findById(spuId);
      if(!rSpu.isSuccess()) {
         log.error("failed to find spu(id={}), error code:{}", spuId, rSpu.getError());
         throw new JsonResponseException(rSpu.getError());
      } else {
         spu.setStockType(((Spu)rSpu.getResult()).getStockType());
         this.extractInfoFromSkus(spu, fullSpu.getSkuTemplates());
         Response<Boolean> rUpdate = this.spuWriter.update(fullSpu);
         if(!rUpdate.isSuccess()) {
            log.error("failed to update {}, error code:{}", fullSpu, rUpdate.getError());
            throw new JsonResponseException(rUpdate.getError());
         } else {
            this.eventBus.post(new SpuUpdatedEvent(spuId));
            return (Boolean)rUpdate.getResult();
         }
      }
   }

   @RequestMapping(
      value = {"/extras"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public boolean updateExtras(@RequestParam(
   name = "id"
) Long id, @RequestParam("extras") String extras) {
      Map<String, String> realTags = Splitter.on(',').withKeyValueSeparator(':').split(extras);
      Response<Boolean> r = this.spuWriteService.extras(id, realTags);
      if(!r.isSuccess()) {
         log.error("failed to update extras to {} for spu(id={}), error code:{} ", new Object[]{extras, id, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         this.eventBus.post(new SpuUpdatedEvent(id));
         return true;
      }
   }

   @RequestMapping(
      value = {"/{id}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   public boolean delete(@PathVariable("id") Long id) {
      Response<Boolean> r = this.spuWriteService.delete(id);
      if(!r.isSuccess()) {
         log.error("failed to delete spu(id={}), error code:{} ", id, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         this.eventBus.post(new SpuDeletedEvent(id));
         return true;
      }
   }
}
