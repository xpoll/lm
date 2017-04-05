package io.terminus.parana.component.spu.component;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.component.dto.spu.EditSpu;
import io.terminus.parana.rule.RuleEngine;
import io.terminus.parana.spu.dto.FullSpu;
import io.terminus.parana.spu.service.SpuReadService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SpuReader {
   private static final Logger log = LoggerFactory.getLogger(SpuReader.class);
   private final SpuReadService spuReadService;
   private final RuleEngine ruleEngine;

   @Autowired
   public SpuReader(SpuReadService spuReadService, RuleEngine ruleEngine) {
      this.spuReadService = spuReadService;
      this.ruleEngine = ruleEngine;
   }

   @Export(
      paramNames = {"spuId"}
   )
   public Response findForEdit(Long spuId) {
      Response<FullSpu> rFullSpu = this.spuReadService.findFullInfoBySpuId(spuId);
      if(!rFullSpu.isSuccess()) {
         log.error("failed to find spu(id={}), error code:{}", spuId, rFullSpu.getError());
         return Response.fail(rFullSpu.getError());
      } else {
         FullSpu fullSpu = (FullSpu)rFullSpu.getResult();

         try {
            EditSpu editSpu = new EditSpu();
            editSpu.setSpu(fullSpu.getSpu());
            editSpu.setSpuDetail(fullSpu.getSpuDetail());
            this.ruleEngine.handleOutboundData(fullSpu, editSpu);
            return Response.ok(editSpu);
         } catch (Exception var5) {
            log.error("failed to find spu(id={}) for edit , cause:{}", spuId, Throwables.getStackTraceAsString(var5));
            return Response.fail("spu.find.fail");
         }
      }
   }
}
