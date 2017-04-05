package io.terminus.parana.component.spu.component;

import com.google.common.base.Throwables;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.parana.rule.RuleEngine;
import io.terminus.parana.rule.dto.BaseOutput;
import io.terminus.parana.spu.dto.FullSpu;
import io.terminus.parana.spu.service.SpuWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
