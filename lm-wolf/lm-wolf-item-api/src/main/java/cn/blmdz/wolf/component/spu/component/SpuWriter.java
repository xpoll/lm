package cn.blmdz.wolf.component.spu.component;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.rule.RuleEngine;
import cn.blmdz.wolf.rule.dto.BaseOutput;
import cn.blmdz.wolf.spu.dto.FullSpu;
import cn.blmdz.wolf.spu.service.SpuWriteService;

@Component
public class SpuWriter {
   private static final Logger log = LoggerFactory.getLogger(SpuWriter.class);
   private final SpuWriteService spuWriteService;
   private final RuleEngine ruleEngine;

   @Autowired
   public SpuWriter(SpuWriteService spuWriteService, RuleEngine ruleEngine) {
      this.spuWriteService = spuWriteService;
      this.ruleEngine = ruleEngine;
   }

   public Response create(FullSpu fullSpu) {
      Long categoryId = fullSpu.getSpu().getCategoryId();

      try {
         this.ruleEngine.handleInboundData(fullSpu, (BaseOutput)null);
         return this.spuWriteService.create(fullSpu);
      } catch (JsonResponseException var4) {
         log.error("failed to validate spu attributes against category(id={}), cause:{}", categoryId, Throwables.getStackTraceAsString(var4));
         return Response.fail(var4.getMessage());
      }
   }

   public Response update(FullSpu fullSpu) {
      Long categoryId = fullSpu.getSpu().getCategoryId();

      try {
         this.ruleEngine.handleInboundData(fullSpu, (BaseOutput)null);
         return this.spuWriteService.update(fullSpu);
      } catch (JsonResponseException var4) {
         log.error("failed to validate spu attributes against category(id={}), cause:{}", categoryId, Throwables.getStackTraceAsString(var4));
         return Response.fail(var4.getMessage());
      }
   }
}
